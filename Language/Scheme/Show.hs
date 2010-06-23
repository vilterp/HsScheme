module Language.Scheme.Show (
  showScheme, toHsStr
) where

import Language.Scheme.Model

instance Show SchemeObj where
  show o = showScheme o

showScheme :: SchemeObj -> String
showScheme (IntObj i) = show i
showScheme (CharObj c) = show c
showScheme EmptyListObj = "()"
showScheme (HandleObj h) = "<Handle>"
showScheme (SymbolObj s) = s
showScheme (PrimProcObj _) = "<Primitive Procedure>"
showScheme (PrimFuncObj _) = "<Primitive Function>"
showScheme cons@(ConsObj a b) | isList cons = if isStr cons then showStr cons else showLst cons
                              | otherwise = showPair cons

toHsStr :: SchemeObj -> Maybe String
toHsStr EmptyListObj = Just ""
toHsStr (ConsObj (CharObj c) cs) = case toHsStr cs of
                                     Just str -> Just $ c : str
                                     Nothing -> Nothing
toHsStr _ = Nothing

showStr :: SchemeObj -> String
showStr l = case toHsStr l of
              Just str -> "\"" ++ str ++ "\""
              Nothing -> error "showStr: not a string"

showLst :: SchemeObj -> String
showLst EmptyListObj = "()"
showLst (ConsObj car cdr) = "(" ++ (showScheme car) ++ (showLst' cdr)
                            where showLst' EmptyListObj = ")"
                                  showLst' (ConsObj car cdr) = " " ++ (showScheme car) ++ (showLst' cdr)
                                  showLst' _ = undefined

showPair :: SchemeObj -> String
showPair (ConsObj a b) = "(" ++ (showScheme a) ++ " . " ++ (showScheme b) ++ ")"
showPair _ = undefined
