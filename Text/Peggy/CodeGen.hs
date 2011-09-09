module Text.Peggy.CodeGen (
  genCode,
  ) where

import Text.Peggy.Syntax

genCode :: Syntax -> String
genCode = undefined . removeLeftRecursion

removeLeftRecursion :: Syntax -> Syntax
removeLeftRecursion = undefined
