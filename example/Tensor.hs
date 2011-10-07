{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
import TensorQQ

main :: IO ()
main = do
  let x = [1, 2, 3]
      i = 1
  print [tens|x_i|]
