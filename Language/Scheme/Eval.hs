module Language.Scheme.Eval (
  eval, evalOne, initEnv, initEnvWithPrelude, loadCode, mkCons
) where

import qualified Data.Map as M
import System.IO
import System.Directory
import System.Vacuum.Ubigraph

import Language.Scheme.Model
import Language.Scheme.Errors
import Language.Scheme.Show
import Language.Scheme.Parse

eval :: PrimProc
eval elo@EmptyListObj _ = return $ Left $ wrongArgsError elo "(object1 [...objectN])"
eval (ConsObj o EmptyListObj) e = evalOne o e
eval (ConsObj o rest) e = do
                            r <- evalOne o e
                            case r of
                              Left e -> return $ Left e
                              Right (res, e') -> eval rest e'
eval o _ = error "call eval with a cons-list of arguments"

evalOne :: PrimProc
evalOne (SymbolObj s) e = return $ case getVar s e of
                                     Left e -> Left e
                                     Right o -> Right (o, e)
evalOne (ConsObj (SymbolObj "if") (ConsObj c (ConsObj i (ConsObj e EmptyListObj)))) env = evalIf c i e env
evalOne def@(ConsObj (SymbolObj "def") (ConsObj (SymbolObj n) (ConsObj args body))) e = return $ if validArgs args then
                                                                                          Right (EmptyListObj, putVar n def e)
                                                                                        else
                                                                                          Left $ invalidForm def
evalOne (ConsObj s@(SymbolObj _) args) e = apply (mkCons [s, args]) e
evalOne c@(ConsObj _ _) e | isStr c = return $ Right (c, e)
                          | isList c = return $ Left $ invalidForm c
                          | otherwise = return $ Right (c, e) -- a pair
evalOne o e = return $ Right (o, e) -- self-evaluating objects

validArgs :: SchemeObj -> Bool
validArgs EmptyListObj = True
validArgs (ConsObj (SymbolObj _) rest) = validArgs rest
validArgs _ = False

invalidForm :: SchemeObj -> Error
invalidForm o = EvalError $ "not a valid form: " ++ (show o)

evalIf :: SchemeObj -> SchemeObj -> SchemeObj -> Env -> ProcRes
evalIf c i e env = do
                     cr <- evalOne c env
                     case cr of
                       Left e -> return $ Left e
                       Right (condRes, env') ->
                         if isTrue condRes then evalOne i env' else evalOne e env'
                   where isTrue (SymbolObj "true") = True
                         isTrue (SymbolObj "false") = False
                         isTrue (IntObj i) = i /= 0
                         isTrue EmptyListObj = False
                         isTrue _ = True

apply :: PrimProc
apply a@(ConsObj (SymbolObj name) (ConsObj args EmptyListObj)) e =
                                            case getVar name e of
                                              Right o -> case o of
                                                PrimProcObj f -> f args e
                                                PrimFuncObj f -> do
                                                  evaldArgs <- consMap evalOne args e
                                                  case evaldArgs of
                                                    Left e -> return $ Left e
                                                    Right (eas, e') -> f eas e'
                                                ConsObj (SymbolObj "def") (ConsObj (SymbolObj n) (ConsObj params body)) -> do
                                                  evaldArgs <- consMap evalOne args e
                                                  case evaldArgs of
                                                    Left e -> return $ Left e
                                                    Right (eas, e') -> do
                                                      r <- applyUDF n params body eas e'
                                                      case r of
                                                        Left e -> return $ Left e
                                                        Right (res, _) -> return $ Right (res, e')
                                                _ -> return $ Left $ EvalError $ "'" ++ (show o) ++ "' is not a procedure"
                                              Left e -> return $ Left e
apply o _ = return $ Left $ EvalError $ "'" ++ (show o) ++ "' is not a valid call"

applyUDF :: String -> SchemeObj -> SchemeObj -> SchemeObj -> Env -> ProcRes
applyUDF name params body args env = let envRes = addToEnv name params args env in case envRes of
                                       Left e -> return $ Left e
                                       Right env' -> eval body env'

addToEnv :: String -> SchemeObj -> SchemeObj -> Env -> Either Error Env
addToEnv name params args e = addToEnv' params args e
                              where addToEnv' EmptyListObj EmptyListObj e = Right e
                                    addToEnv' (ConsObj (SymbolObj n) ns) (ConsObj v vs) e = addToEnv' ns vs (putVar n v e)
                                    addToEnv' _ _ _ = Left $ wrongArgsError args (show params)

consMap :: PrimProc -> PrimProc
consMap _ EmptyListObj e = return $ Right (EmptyListObj, e)
consMap f (ConsObj x xs) e = do
                               res <- f x e
                               case res of
                                 Left e -> return $ Left e
                                 Right (r, e') -> do
                                   restRes <- consMap f xs e'
                                   case restRes of
                                     Left e -> return $ Left e
                                     Right (rest, _) -> return $ Right (ConsObj r rest, e')

getVar :: String -> Env -> Either Error SchemeObj
getVar n e = case M.lookup n e of
               Just o -> Right o
               Nothing -> Left $ EvalError $ "unbound variable '" ++ n ++ "'"

putVar :: String -> SchemeObj -> Env -> Env
putVar name val e = M.insert name val e

initEnvWithPrelude :: IO (Either Error Env)
initEnvWithPrelude = do
                       let args = mkCons [mkStr "Prelude.hscm"]
                       res <- loadCode args initEnv
                       case res of
                         Left e -> return $ Left e
                         Right (_, env) -> return $ Right env

initEnv :: Env
initEnv = M.fromList $ primProcs ++ primFuncs ++ vars
          where vars = [("stdin", HandleObj stdin), ("stdout", HandleObj stdout)]
                primProcs = map mkPPO $ [("set", setVar), ("quote", quote), ("eval", eval), ("apply", apply)]
                primFuncs = map mkPFO $ arith ++ test ++ io ++ logic ++ listPrims ++ misc
                arith = map mkAf [("+", (+)), ("-", (-)), ("*", (*)), ("/", quot), ("^", (^)), ("%", mod)]
                test = map mkTf [("==", testEq), ("!=", testNeq), (">", testGt), (">=", testGte), ("<", testLt), ("<=", testLte)]
                listPrims = [("car", car), ("cdr", cdr), ("nil?", isNil), ("cons", cons)]
                logic = map mkLo [("or", (||)), ("and", (&&))]
                io = [("read-char", readChar), ("write-char", writeChar), ("open-file", openF), ("close-file", closeF), ("at-eof?", atEof), ("load", loadCode)]
                misc = [("read", readCode), ("show", showCode), ("error", raiseError), ("view", vacuumView)]
                mkLo (n, f) = (n, logicOp f)
                mkTf (n, f) = (n, testFun n f)
                mkAf (n, f) = (n, arithFun f)
                mkPPO (n, p) = (n, PrimProcObj p)
                mkPFO (n, f) = (n, PrimFuncObj f)

wrongArgsError :: SchemeObj -> String -> Error
wrongArgsError o msg = EvalError $ "invalid arguments. Given " ++ (show o) ++ "; should be: " ++ msg

-- PrimProcs

setVar :: PrimProc
setVar (ConsObj (SymbolObj name) (ConsObj val EmptyListObj)) e = do
                                                                   res <- evalOne val e
                                                                   return $ case res of
                                                                     Left e -> Left e
                                                                     Right (so, e') -> Right (EmptyListObj, putVar name so e')
setVar o _ = return $ Left $ wrongArgsError o "(string object)"

arithFun :: (Int -> Int -> Int) -> PrimProc
arithFun f args e = return $ case args of
                               ConsObj (IntObj a) (ConsObj (IntObj b) EmptyListObj) -> Right (IntObj $ f a b, e)
                               o -> Left $ wrongArgsError o "(int int)"

raiseError :: PrimProc
raiseError o@(ConsObj str EmptyListObj) e = return $ case toHsStr str of
                                            Just msg -> Left $ EvalError msg
                                            Nothing -> Left $ wrongArgsError o "(string)"
raiseError o e = return $ Left $ wrongArgsError o "(string)"

-- read & show

readCode :: PrimProc
readCode = undefined

showCode :: PrimProc
showCode (ConsObj o EmptyListObj) e = return $ Right (mkStr $ show o, e)
showCode o e = return $ Left $ wrongArgsError o "(object)"

-- List prims

car :: PrimProc
car (ConsObj (ConsObj a _) EmptyListObj) e = return $ Right (a, e)
car o _ = return $ Left $ wrongArgsError o "(cons)"

cdr :: PrimProc
cdr (ConsObj (ConsObj _ b) EmptyListObj) e = return $ Right (b, e)
cdr o _ = return $ Left $ wrongArgsError o "(cons)"

isNil :: PrimProc
isNil (ConsObj EmptyListObj EmptyListObj) e = return $ Right (SymbolObj "true", e)
isNil (ConsObj (ConsObj _ _) EmptyListObj) e = return $ Right (SymbolObj "false", e)
isNil o _ = return $ Left $ wrongArgsError o "(list)"

cons :: PrimProc
cons (ConsObj a (ConsObj b EmptyListObj)) e = return $ Right (ConsObj a b, e)
cons o _ = return $ Left $ wrongArgsError o "(object object)"

-- Logic

logicOp :: (Bool -> Bool -> Bool) -> PrimProc
logicOp f args e = return $ case extract2bool args of
                     Just (a, b) -> Right (mkBool $ f a b, e)
                     Nothing -> Left $ wrongArgsError args "(bool bool)"
                   where extract2bool (ConsObj (SymbolObj a) (ConsObj (SymbolObj b) EmptyListObj)) = extract a b
                         extract2bool _ = Nothing
                         extract a b = case ext a of
                                         Just x -> case ext b of
                                           Just y -> Just (x, y)
                                           Nothing -> Nothing
                                         Nothing -> Nothing
                         ext "true" = Just True
                         ext "false" = Just False
                         ext _ = Nothing

mkBool :: Bool -> SchemeObj
mkBool True = SymbolObj "true"
mkBool False = SymbolObj "false"

-- Tests

type TestFun = Ordering -> Bool

testFun :: String -> TestFun -> PrimProc
testFun name f args e = return $ case args of
                          ConsObj a (ConsObj b EmptyListObj) ->
                            case soCmp a b of
                              Just ord -> Right (if f ord then SymbolObj "true" else SymbolObj "false", e)
                              Nothing -> Left $ EvalError $ "invalid arguments to " ++ name ++ ": " ++ (show args)
                          o -> Left $ wrongArgsError o "(object object)"

soCmp :: SchemeObj -> SchemeObj -> Maybe Ordering
soCmp (IntObj a) (IntObj b) = Just $ compare a b
soCmp (CharObj a) (CharObj b) = Just $ compare a b
soCmp EmptyListObj EmptyListObj = Just $ EQ
soCmp (ConsObj _ _) EmptyListObj = Just $ GT
soCmp EmptyListObj (ConsObj _ _) = Just $ LT
soCmp (ConsObj a as) (ConsObj b bs) = case soCmp a b of
                                        Just EQ -> soCmp as bs
                                        res -> res
soCmp _ _ = Nothing

testEq, testNeq, testGt, testLt, testGte, testLte :: TestFun

testEq EQ = True
testEq _ = False

testNeq EQ = False
testNeq _ = True

testGt GT = True
testGt _ = False

testGte GT = True
testGte EQ = True
testGte _ = False

testLt LT = True
testLt _ = False

testLte LT = True
testLte EQ = True
testLte _ = False

quote :: PrimProc
quote (ConsObj o EmptyListObj) e = return $ Right (o, e)
quote o _ = return $ Left $ wrongArgsError o "(object)"

readChar :: PrimProc
readChar (ConsObj (HandleObj h) EmptyListObj) e = do
                            c <- hGetChar h
                            return $ Right $ (CharObj c, e)
readChar o _ = return $ Left $ wrongArgsError o "(handle)"

writeChar :: PrimProc
writeChar (ConsObj (CharObj c) (ConsObj (HandleObj h) EmptyListObj)) e = do
                                                                           hPutChar h c
                                                                           return $ Right (EmptyListObj, e)
writeChar o _ = return $ Left $ wrongArgsError o "(char handle)"

openF :: PrimProc
openF o@(ConsObj str (ConsObj (SymbolObj md) EmptyListObj)) e = case toHsStr str of
                                                                    Just path -> case extractMode md of
                                                                      Just mode -> do
                                                                        case mode of
                                                                          ReadMode -> do
                                                                            exists <- doesFileExist path
                                                                            case exists of
                                                                              True -> returnFile path mode
                                                                              False -> return $ Left $ EvalError $ "file doesn't exist: " ++ path
                                                                          _ -> returnFile path mode
                                                                      Nothing -> return $ Left $ ofWrongArgs o
                                                                    Nothing -> return $ Left $ ofWrongArgs o
                                                                where extractMode "read" = Just ReadMode
                                                                      extractMode "write" = Just WriteMode
                                                                      extractMode "append" = Just AppendMode
                                                                      extractMode "read-write" = Just ReadWriteMode
                                                                      extractMode _ = Nothing
                                                                      returnFile path mode = do
                                                                        handle <- openFile path mode
                                                                        return $ Right (HandleObj handle, e)
openF o _ = return $ Left $ ofWrongArgs o

ofWrongArgs o = wrongArgsError o "(string mode-symbol)"

closeF :: PrimProc
closeF (ConsObj (HandleObj h) EmptyListObj) e = do
                                                  hClose h
                                                  return $ Right (EmptyListObj, e)
closeF o _ = return $ Left $ wrongArgsError o "(handle)"

atEof :: PrimProc
atEof (ConsObj (HandleObj h) EmptyListObj) e = do
                                                 open <- hIsOpen h
                                                 case open of
                                                   True -> do
                                                     res <- hIsEOF h
                                                     return $ Right (mkBool res, e)
                                                   False -> return $ Left $ EvalError "handle is closed"
atEof o _ = return $ Left $ wrongArgsError o "(handle)"

vacuumView :: PrimProc
vacuumView (ConsObj o EmptyListObj) e = do
                                         view o
                                         return $ Right (EmptyListObj, e)
vacuumView o _ = return $ Left $ wrongArgsError o "(object)"

loadCode :: PrimProc
loadCode args@(ConsObj p EmptyListObj) e = do
                                   case toHsStr p of
                                     Just path -> do
                                       exists <- doesFileExist path
                                       case exists of
                                         True -> do
                                           contents <- readFile path
                                           case readProgram contents path of
                                             Right sos -> eval (mkCons sos) e
                                             Left e -> return $ Left e
                                         False -> return $ Left $ EvalError $ "file doesn't exist: " ++ path
                                     Nothing -> wrongIcArgs args
loadCode o _ = wrongIcArgs o

wrongIcArgs o = return $ Left $ wrongArgsError o "(path [...paths])"

