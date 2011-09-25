{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Main (main) where

import Text.Peggy

genParser [("qqexpr", "top")] [peggy|
-- Simple Arithmetic Expression Parser

top :: Double = expr

expr :: Double
  = expr "+" fact { $1 + $2 }
  / expr "-" fact { $1 - $2 }
  / fact

fact :: Double
  = fact "*" term { $1 * $2 }
  / fact "/" term { $1 / $2 }
  / term

term :: Double
  = "(" expr ")"
  / number

number ::: Double
  = [1-9] [0-9]* { read ($1 : $2) }
|]

main :: IO ()
main = print . parse top (SrcPos "<stdin>" 0 1 1) =<< getContents
