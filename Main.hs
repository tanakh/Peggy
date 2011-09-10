{-# Language QuasiQuotes #-}

module Main (main) where

import Text.Peggy
import Text.Peggy.SrcLoc
import Text.Peggy.Quote

[peggy|
-- Simple Arithmetic Expression Parser

expr : Double
  = expr "+" fact { $1 + $2 }
  / expr "-" fact { $1 - $2 }
  / fact

fact : Double
  = fact "*" term { $1 * $2 }
  / fact "/" term { $1 / $2 }
  / term

term : Double
  = "(" expr ")"
  / number

number : Double
  = [1-9] [0-9]* { read ($1 : $2) }
|]

main :: IO ()
main = do
  con <- getContents
  print $ unParser expr $ parse (SrcLoc "<stdin>" 0 1 1) con
