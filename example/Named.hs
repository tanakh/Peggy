{-# Language QuasiQuotes #-}

import Text.Peggy
import Text.Peggy.Quote

[peggy|
test :: Int
  = x:foo "-" y:foo { y-x } 

foo :: Int
  = s:[0-9]+ { read s }
|]

main :: IO ()
main = do
  print . runParser test "<stdin>" $ "1-2"
  print . runParser test "<stdin>" $ "5-3"
