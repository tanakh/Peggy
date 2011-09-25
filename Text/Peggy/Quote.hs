{-# Language RankNTypes #-}

module Text.Peggy.Quote (
  peggy,
  peggyFile,
  
  genParser,
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.Pos

import Text.Peggy.Parser
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc
import Text.Peggy.CodeGen.TH

peggy :: QuasiQuoter
peggy = QuasiQuoter { quoteDec = qDecs, quoteExp = qExp, quotePat = undefined, quoteType = undefined }

peggyFile :: FilePath -> Q Exp
peggyFile filename = do
  txt <- runIO $ readFile filename
  case parse syntax filename txt of
    Left err -> error $ show err
    Right syn -> dataToExpQ (const Nothing) syn

qDecs :: String -> Q [Dec]
qDecs txt = do
  loc <- location
  genDecs $ parseSyntax (SrcPos (loc_filename loc) 0 (fst $ loc_start loc) (snd $ loc_start loc)) txt

qExp :: String -> Q Exp
qExp txt = do
  loc <- location
  dataToExpQ (const Nothing) $ parseSyntax (SrcPos (loc_filename loc) 0 (fst $ loc_start loc) (snd $ loc_start loc)) txt

genParser :: [(String, String)] -> Syntax -> Q [Dec]
genParser qqs syn = do
  qq <- mapM (genQQ syn) qqs
  dec <- genDecs syn
  return $ concat qq ++ dec

--

parseSyntax :: SrcPos -> String -> Syntax
parseSyntax (SrcPos fname _ lno cno) txt =
  case parse (setPosition (newPos fname lno cno) >> syntax) fname txt of
    Left err -> error $ "peggy syntax-error: " ++ show err
    Right defs -> defs
