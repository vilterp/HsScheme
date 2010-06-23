module Language.Scheme.Parse (
  sExpr, readProgram, isDone
) where

import Language.Scheme.Lex
import Language.Scheme.Model
import Language.Scheme.Errors

import Parseclone.Core
import Parseclone.LexPrims

-- INTERFACE

readProgram :: String -> FilePath -> Either Error [SchemeObj]
readProgram i fp = case run lexScheme i fp of
                     Right (tokens, _) -> case run (phrase program) tokens fp of
                       Right (objs, _) -> Right objs
                       Left pe -> Left $ ParseError $ getMsg pe
                     Left pe -> Left $ LexError $ getMsg pe

-- PARSER

type SchemeParser = Parser Token SchemeObj

program :: Parser Token [SchemeObj]
program = rep1 sExpr

sExpr :: SchemeParser
sExpr = atom `eitherOr` list `eitherOr` pair `eitherOr` quotedForm

atom :: SchemeParser
atom = oneOfP "atom" [int, symbol, char, string]

int :: SchemeParser
int = (literalP "int token" isIntT) `wrap` \(IntT i) -> IntObj i

symbol :: SchemeParser
symbol = (literalP "symbol token" isSymbolT) `wrap` \(SymbolT s) -> SymbolObj s

char :: SchemeParser
char = (literalP "char token" isCharT) `wrap` \(CharT c) -> CharObj c

string :: SchemeParser
string = (literalP "string token" isStringT) `wrap` \(StringT s) -> foldr ConsObj EmptyListObj (map CharObj s)

list :: SchemeParser
list = open `andThen` (rep sExpr) `andThen` close `wrap`
          \((_, ses), _) -> foldr ConsObj EmptyListObj ses

pair :: SchemeParser
pair = open `andThen` sExpr `andThen` (literalP "dot" isDot) `andThen` sExpr `andThen` close `wrap` mkCons
       where isDot (DelimT ".") = True
             isDot _ = False
             mkCons ((((_, a), _), b), _) = ConsObj a b

open :: Parser Token Token
open = literalP "open paren" isOpenT

close :: Parser Token Token
close = literalP "close paren" isCloseT

singleQuote :: Parser Token Token
singleQuote = literalP "single quote" isQuote
              where isQuote (DelimT "'") = True
                    isQuote _ = False

quotedForm :: SchemeParser
quotedForm = singleQuote `andThen` sExpr `wrap` \(_, o) -> ConsObj (SymbolObj "quote") (ConsObj o EmptyListObj)

