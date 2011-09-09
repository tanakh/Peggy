module Text.Peggy.SrcLoc (
  SrcLoc(..),
  SrcSpan,
  
  advance,
  ) where

data SrcLoc =
  SrcLoc
  { locFile :: FilePath
  , locAbs  :: !Int
  , locLine :: !Int
  , locCol  :: !Int
  }
  deriving (Show)

type SrcSpan = (SrcLoc, SrcLoc)

tabWidth :: Int
tabWidth = 8

advance :: SrcLoc -> Char -> SrcLoc
advance (SrcLoc f a l c) x =
  case x of
    '\t' -> SrcLoc f (a + 1) l ((c - 1 + tabWidth - 1) `div` tabWidth * tabWidth + 1)
    '\n' -> SrcLoc f (a + 1) (l + 1) 1
    _    -> SrcLoc f (a + 1) l (c + 1)
