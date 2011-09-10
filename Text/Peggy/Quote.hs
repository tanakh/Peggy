module Text.Peggy.Quote (
  peggy,
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec

import Text.Peggy.Parser
import Text.Peggy.CodeGen

peggy :: QuasiQuoter
peggy = QuasiQuoter { quoteDec = quote }

quote :: String -> Q [Dec]
quote txt =
  case parse syntax "<quasi-quoter>" txt of
    Left err -> error $ "peggy syntax-error: " ++ show err
    Right defs ->
      return $ genCode defs
