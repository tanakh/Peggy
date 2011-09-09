module Main (main) where

import Control.Monad
import System.Environment
import Text.Parsec.String

import Text.Peggy
import Text.Peggy.SrcLoc
import Text.Peggy.Parser

import Prelude hiding (exp)

main :: IO ()
main = do
  [fname] <- getArgs
  res <- parseFromFile syntax fname
  case res of
    Left err -> print err
    Right defs -> do
      forM_ defs print
