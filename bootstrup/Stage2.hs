{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Stage2 where

import qualified Stage1

import Data.Char
import Numeric
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Peggy

genParser [] $(runIO (parseFile Stage1.syntax "peggy.peggy") >>= \res -> case res of Left err -> error $ show err; Right syn -> dataToExpQ (const Nothing) syn)
