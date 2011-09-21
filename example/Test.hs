{-# Language QuasiQuotes #-}
{-# Language FlexibleContexts #-}

import Text.Peggy.Quote
import Text.Peggy.PrimST

[peggy|
skip :: String = 'a'
|]

main :: IO ()
main =  print .  parse skip =<< getContents
