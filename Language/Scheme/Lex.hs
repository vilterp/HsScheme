module Language.Scheme.Lex (
  Token(..), lexScheme, isDone,
  isDelimT, isStringT, isSymbolT, isCharT, isIntT, isOpenT, isCloseT
) where

import Parseclone.Core
import Parseclone.LexPrims

import Data.Maybe
import Data.Char

data Token = StringT String
           | SymbolT String
           | IntT Int
           | CharT Char
           | OpenT
           | CloseT
           | DelimT String

instance Show Token where
  show (StringT s) = show s
  show (SymbolT s) = s
  show (IntT i) = show i
  show (CharT c) = show c
  show OpenT = "("
  show CloseT = ")"
  show (DelimT s) = s

isDone :: [Token] -> Bool
isDone ts = isDone' ts 0
            where isDone' [] n = n <= 0
                  isDone' (t:ts) n = case t of
                                       OpenT -> isDone' ts (n+1)
                                       CloseT -> isDone' ts (n-1)
                                       _ -> isDone' ts n

type TokTest = Token -> Bool

-- could use macros here...

isDelimT :: TokTest
isDelimT (DelimT _) = True
isDelimT _ = False

isStringT :: TokTest
isStringT (StringT _) = True
isStringT _ = False

isSymbolT :: TokTest
isSymbolT (SymbolT _) = True
isSymbolT _ = False

isCharT :: TokTest
isCharT (CharT _) = True
isCharT _ = False

isIntT :: TokTest
isIntT (IntT _) = True
isIntT _ = False

isOpenT :: TokTest
isOpenT OpenT = True
isOpenT _ = False

isCloseT :: TokTest
isCloseT CloseT = True
isCloseT _ = False

-- actual lexer rules

lexScheme :: Lexer [Token]
lexScheme = (phrase $ rep token) `wrap` catMaybes

token :: Lexer (Maybe Token)
token = whiteSpace `eitherOr` (actualToken `wrap` Just)

whiteSpace :: Lexer (Maybe Token)
whiteSpace = ((rep1 $ literalP "whitespace" isSpace) `eitherOr` comment) `wrap` \_ -> Nothing

comment :: Lexer String
comment = (literal ';') `andThen` (literal ';') `andThen`
                (rep $ literalP "comment contents" (/= '\n')) `wrap` \((_, _), cm) -> cm

actualToken :: Lexer Token
actualToken = oneOfP "token" [stringTok, intTok, symbolTok, charTok, openTok, closeTok, delimeter]

symbolTok :: Lexer Token
symbolTok = normalSymbol `eitherOr` operator

normalSymbol :: Lexer Token
normalSymbol = letter `andThen` (rep (letter `eitherOr` digit `eitherOr` (oneOf "symbol character" "-?!"))) `wrap`
                     \(x, xs) -> SymbolT $ x:xs

operator :: Lexer Token
operator = (singleAndMulti "operator symbol" ["==", "!=", ">=", "<="] "+-*/^%><") `wrap` SymbolT

intTok :: Lexer Token
intTok = (opt $ literal '-') `andThen` (rep1 digit) `wrap` \(neg, digits) -> IntT $ read $ (maybeToList neg) ++ digits

stringTok :: Lexer Token
stringTok = stringLit `wrap` StringT

charTok :: Lexer Token
charTok = charLit `wrap` CharT

openTok :: Lexer Token
openTok = literal '(' `wrap` \_ -> OpenT

closeTok :: Lexer Token
closeTok = literal ')' `wrap` \_ -> CloseT

delimeter :: Lexer Token
delimeter = (singleAndMulti "delimeter" [",@"] ".,`'") `wrap` DelimT

