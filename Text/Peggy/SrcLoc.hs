module Text.Peggy.SrcLoc (
  SrcLoc(..),
  SrcPos(..),
  
  advance,
  ) where

data SrcLoc
  = LocPos  SrcPos
  | LocSpan SrcPos SrcPos
  deriving (Show)

data SrcPos =
  SrcPos
  { locFile :: FilePath
  , locAbs  :: !Int
  , locLine :: !Int
  , locCol  :: !Int
  }
  deriving (Show)

tabWidth :: Int
tabWidth = 8

advance :: SrcPos -> Char -> SrcPos
advance (SrcPos f a l c) x =
  case x of
    '\t' -> SrcPos f (a + 1) l ((c - 1 + tabWidth - 1) `div` tabWidth * tabWidth + 1)
    '\n' -> SrcPos f (a + 1) (l + 1) 1
    _    -> SrcPos f (a + 1) l (c + 1)
