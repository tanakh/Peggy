{-# Language QuasiQuotes #-}

import Text.Peggy
import Text.Peggy.SrcLoc
import Text.Peggy.Quote

[peggy|
program :: ()
  = (!"/*" !"*/" . { () } / comment)* !. { () }
 
comment :: ()
  = "/*" commentElem* "*/" { () }

commentElem :: ()
  = comment
  / !"*/" . { () }
|]

main :: IO ()
main = do
  con <- getContents
  print $ unParser program $ parse (SrcPos "<stdin>" 0 1 1) con
