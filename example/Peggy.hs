{-# Language QuasiQuotes #-}

import Data.Char
import Numeric
import Text.Peggy
import Text.Peggy.Syntax

[peggy_f|peggy.peggy|]

main :: IO ()
main = print . runParser syntax "<stdin>" =<< getContents
