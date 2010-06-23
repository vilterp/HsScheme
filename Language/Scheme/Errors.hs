module Language.Scheme.Errors (
  Error(..), dispError
) where

data Error = LexError String
           | ParseError String
           | EvalError String
           deriving Show

dispError :: Error -> String
dispError (ParseError e) = "Parse Error: " ++ e
dispError (LexError e) = "Lex Error: " ++ e
dispError (EvalError e) = "Evaluation Error: " ++ e

