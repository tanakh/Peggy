{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
import Text.Peggy

data Number = Number SrcLoc Int deriving (Show)

[peggy|
nums :: [Number]
  = num*
num ::: Number
  = [0-9]+ { Number $p (read $1) }
|]

main :: IO ()
main =
  case parseString nums "" "12    2434 \n  3 4 576" of
    Left err -> print err
    Right ns -> mapM_ print ns
