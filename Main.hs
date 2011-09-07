module Main (main) where

import Text.Peggy

import Control.Applicative
import Data.Char

import Prelude hiding (exp)

exp :: ParserT IO Double
exp =
  (+) <$> fact <* (string "+") <*> exp <|>
  fact

fact :: ParserT IO Double
fact =
  (*) <$> term <* (string "*") <*> fact <|>
  term

term :: ParserT IO Double
term = digit

digit :: ParserT IO Double
digit = read <$> some (satisfyChar isDigit)

main :: IO ()
main = do
  str <- getContents
  print =<< runParser exp str
