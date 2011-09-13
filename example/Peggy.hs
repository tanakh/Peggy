{-# Language QuasiQuotes #-}

import Data.Char
import Numeric
import Text.Peggy
import Text.Peggy.Syntax
import Text.Peggy.Quote

[peggy_f|peggy.peggy|]

main :: IO ()
main = print . runParser syntax "<stdin>" =<< getContents
