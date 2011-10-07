{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Data.Char
import Numeric
import Language.Haskell.TH
import Language.Haskell.Meta.Utils

import qualified Stage2

import Text.Peggy.Prim
import Text.Peggy.Quote
import Text.Peggy.CodeGen.TH
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc

header :: String
header =
  unlines
  [ "{-# LANGUAGE RankNTypes, FlexibleContexts #-}"
  , ""
  , "module Text.Peggy.Parser (syntax) where"
  , "import Control.Applicative"
  , "import Data.ListLike.Base hiding (head)"
  , "import Data.HashTable.ST.Basic"
  , "import Numeric"
  , "import Data.Char"
  , "import Text.Peggy.Prim"
  , "import Text.Peggy.Syntax"
  ]

main :: IO ()
main = do
  res <- parseFile Stage2.syntax "./peggy.peggy"
  case res of
    Left err -> error $ show err
    Right defs -> do
      code <- runQ $ genDecs defs
      putStrLn header
      putStrLn $ pp code
