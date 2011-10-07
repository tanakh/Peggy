{-# LANGUAGE QuasiQuotes #-}

import Text.Peggy

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
