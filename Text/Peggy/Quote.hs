module Text.Peggy.Quote (
  peggy,
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.Pos

import Text.Peggy.Parser
import Text.Peggy.CodeGen

peggy :: QuasiQuoter
peggy = QuasiQuoter { quoteDec = quote, quoteExp = undefined, quotePat = undefined, quoteType = undefined }

quote :: String -> Q [Dec]
quote txt = do
  loc <- location
  case parse (setPosition (initPos loc) >> syntax) (loc_filename loc) txt of
    Left err -> error $ "peggy syntax-error: " ++ show err
    Right defs -> genCode defs
  where
    initPos loc = newPos (loc_filename loc) (fst $ loc_start loc) (snd $ loc_start loc)
