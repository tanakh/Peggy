module Main (main) where

import Text.Peggy
import Text.Peggy.SrcLoc

import Control.Applicative
import Data.Char

import Prelude hiding (exp)

type P a = Parser ArithDerivs a

expr :: P Double
expr =
  (+) <$> Parser advFact <* (string "+") <*> Parser advExpr <|>
  Parser advFact

fact :: P Double
fact =
  (*) <$> Parser advTerm <* (string "*") <*> Parser advFact <|>
  Parser advTerm

term :: P Double
term = Parser advDigit

digit :: P Double
digit = read <$> some (satisfy isDigit)

data ArithDerivs =
  ArithDerivs
  { advExpr :: Result ArithDerivs Double
  , advFact :: Result ArithDerivs Double
  , advTerm :: Result ArithDerivs Double
  , advDigit :: Result ArithDerivs Double
  , advChar :: Result ArithDerivs Char
  , advPos :: SrcLoc
  }

instance Derivs ArithDerivs where
  dvPos = advPos
  dvChar = advChar

parse :: SrcLoc -> String -> ArithDerivs
parse pos str = d where
  d = ArithDerivs
      (unParser expr d)
      (unParser fact d)
      (unParser term d)
      (unParser digit d)
      pchar
      pos
  
  pchar =
    case str of
      c:cs ->
        Parsed (parse (pos `advance` c) cs) c
      _ ->
        Failed $ ParseError (dvPos d) ""

main :: IO ()
main = do
  str <- getContents
  print $ unParser expr (parse (SrcLoc "<stdin>" 0 1 1) str)
