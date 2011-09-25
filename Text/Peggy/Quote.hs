{-# Language RankNTypes #-}

module Text.Peggy.Quote (
  peggy,
  peggyFile,
  
  genParser,
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.Peggy.Parser
import Text.Peggy.Prim
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc
import Text.Peggy.CodeGen.TH

peggy :: QuasiQuoter
peggy = QuasiQuoter { quoteDec = qDecs, quoteExp = qExp, quotePat = undefined, quoteType = undefined }

peggyFile :: FilePath -> Q Exp
peggyFile filename = do
  res <- runIO $ parseFile syntax filename
  case res of
    Left err -> error $ show err
    Right syn -> dataToExpQ (const Nothing) syn

qDecs :: String -> Q [Dec]
qDecs txt = do
  loc <- location
  genDecs $ parseSyntax (locToPos loc) txt

qExp :: String -> Q Exp
qExp txt = do
  loc <- location
  dataToExpQ (const Nothing) $ parseSyntax (locToPos loc) txt

genParser :: [(String, String)] -> Syntax -> Q [Dec]
genParser qqs syn = do
  qq <- mapM (genQQ syn) qqs
  dec <- genDecs syn
  return $ concat qq ++ dec

--

parseSyntax :: SrcPos -> String -> Syntax
parseSyntax pos txt =
  case parse syntax pos txt of
    Left err -> error $ "peggy syntax-error: " ++ show err
    Right defs -> defs

locToPos :: Loc -> SrcPos
locToPos loc =
  SrcPos (loc_filename loc) 0 (fst $ loc_start loc) (snd $ loc_start loc)
