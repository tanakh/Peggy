{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Stage1 where

import Data.Char
import Language.Haskell.TH.Quote
import Numeric
import Text.Peggy

genParser [] $(peggyFile "peggy.peggy")
