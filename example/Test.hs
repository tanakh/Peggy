{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

import Text.Peggy

[peggy|
skip :: String = 'a'
|]

main :: IO ()
main =  print .  parse skip =<< getContents
