module Main where

import System.Environment
import Control.Monad
import System.Exit
import System.Console.Haskeline
import Control.Monad.Trans

import Parseclone.Core

import Language.Scheme.Errors
import Language.Scheme.Model
import Language.Scheme.Lex
import Language.Scheme.Parse
import Language.Scheme.Eval
import Language.Scheme.Show

main = do
         args <- getArgs
         r <- initEnvWithPrelude
         case r of
           Left e -> putStrLn $ dispError e
           Right initEnv -> case length args of
             0 -> do
               putStrLn "Welcome to Scheme"
               runInputT defaultSettings (replLoop $ initSt initEnv)
             1 -> do
               r <- loadFile $ head args
               case r of
                 Right sos -> foldM_ process initEnv sos
                              where process e so = do
                                                     res <- evalOne so e
                                                     case res of
                                                       Left err -> do
                                                         putStrLn $ dispError err
                                                         exitWith $ ExitFailure 1
                                                       Right (_, e') -> return e'
                 Left e -> putStrLn $ dispError e
             _ -> do
                    putStrLn "usage: \"hsscheme\" for interactive prompt"
                    putStrLn "       \"hsscheme myfile.hscm\" to run file"

replLoop :: ReplState -> InputT IO ()
replLoop st = do
                let prompt = if isInContinuation st then ">> " else " | "
                maybeLine <- getInputLine prompt
                case maybeLine of
                  Nothing -> return ()
                  Just line -> do
                    result <- liftIO $ processRepl line st
                    case result of
                      (Right mbs, st') -> case mbs of
                        Just s -> do
                          outputStr "=> "
                          outputStrLn s
                          replLoop st'
                        Nothing -> replLoop st'
                      (Left e, st') -> do
                                         outputStrLn $ dispError e
                                         replLoop st'

-- repl stuff

data ReplState = ReplState { buffer :: [Token], env :: Env }

initSt :: Env -> ReplState
initSt e = ReplState [] e

isInContinuation :: ReplState -> Bool
isInContinuation st = null $ buffer st

processRepl :: String -> ReplState -> IO (Either Error (Maybe String), ReplState)
processRepl cmd st = case run lexScheme cmd fn of
                       Right (toks, _) ->
                          let buffer' = (buffer st) ++ toks in
                          case isDone buffer' of
                            True -> do
                              let st' = st { buffer = [] }
                                  parseRes = run (phrase sExpr) buffer' fn
                              case parseRes of
                                Left e -> return (Left $ ParseError $ getMsg e, st')
                                Right (so, _) -> do
                                  evalRes <- evalOne so (env st)
                                  case evalRes of
                                    Left e -> return (Left e, st')
                                    Right (resObj, env') -> return (Right $ Just $ showScheme resObj, st' { env = env' })
                            False -> return (Right Nothing, st { buffer = buffer' })
                       Left e -> return (Left $ LexError $ getMsg e, st { buffer = [] })

fn = "<console>"

-- run file stuff

loadFile :: FilePath -> IO (Either Error [SchemeObj])
loadFile fp = do
                contents <- readFile fp
                return $ readProgram contents fp

