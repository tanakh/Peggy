module Text.Peggy.Normalize (
  normalize,
  ) where

import Text.Peggy.Syntax

normalize :: Syntax -> Syntax
normalize = id
