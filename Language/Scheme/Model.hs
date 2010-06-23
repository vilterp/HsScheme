{-# LANGUAGE TypeSynonymInstances #-}

module Language.Scheme.Model (
  SchemeObj(..), Env, PrimProc, ProcRes,
  objDesc, isStr, isList, mkCons, mkStr
) where

import System.IO
import Data.Map

import Language.Scheme.Errors

type Env = Map String SchemeObj
type ProcRes = IO (Either Error (SchemeObj, Env))
type PrimProc = SchemeObj -> Env -> ProcRes

instance Show PrimProc where
  show _ = "PrimProc"

instance Eq PrimProc where
  a == b = False

data SchemeObj = IntObj Int
               | CharObj Char
               | SymbolObj String
               | ConsObj SchemeObj SchemeObj
               | EmptyListObj
               | HandleObj Handle
               | PrimProcObj PrimProc
               | PrimFuncObj PrimProc
               deriving (Eq)

objDesc :: SchemeObj -> String
objDesc (IntObj _) = "an integer"
objDesc (CharObj _) = "a character"
objDesc (SymbolObj _) = "a symbol"
objDesc (ConsObj _ _) = "a cons"
objDesc EmptyListObj = "the empty list"
objDesc (HandleObj _) = "a handle"
objDesc (PrimProcObj _) = "a primitive procedure"
objDesc (PrimFuncObj _) = "a primitive function"

isList :: SchemeObj -> Bool
isList EmptyListObj = True
isList (ConsObj _ cdr) = isList cdr
isList _ = False

isStr :: SchemeObj -> Bool
isStr EmptyListObj = True
isStr (ConsObj (CharObj _) cdr) = isStr cdr
isStr _ = False

mkCons :: [SchemeObj] -> SchemeObj
mkCons [] = EmptyListObj
mkCons (x:xs) = ConsObj x (mkCons xs)

mkStr :: String -> SchemeObj
mkStr [] = EmptyListObj
mkStr (x:xs) = ConsObj (CharObj x) (mkStr xs)

