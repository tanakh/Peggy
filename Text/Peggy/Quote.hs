{-# Language RankNTypes #-}

-- |
-- Module      : Text.Peggy.Quote
-- Copyright   : (c) Hideyuki Tanaka 2011
-- License     : BSD-style
--
-- Maintainer  : tanaka.hideyuki@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The quasi-quoters of peggy syntax.
--

module Text.Peggy.Quote (
  -- * Quasi Quoters
  peggy,
  peggyFile,
  
  -- * Parser and Quasi-quoter generating function
  genParser,
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Text.Peggy.Parser
import Text.Peggy.Prim
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc
import Text.Peggy.CodeGen.TH

-- | quasi-quoter for peggy syntax
-- When it is used at top-level of source code,
-- definitions of parsers are generated.
--
-- > {-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
-- > import Text.Peggy
-- > 
-- > [peggy|
-- > foo :: [Int]
-- >   = num*
-- > num ::: Int
-- >   = [0-9]+ { read $1 }
-- > |]
-- > 
-- > main :: IO ()
-- > main = print . parseString foo "<stdin>" =<< getContents
--
-- When it is used as expression,
-- the result value is 'Syntax'.
--
-- > main = print [peggy|
-- > num :: Int
-- >   = [0-9]+ { read $1 }
-- > |]
--
-- The result is:
--
-- > $ runhaskell Test.hs
-- > [Definition "num" "Int\n  " (Choice [Semantic (Sequence [Some (TerminalSet [CharRange '0' '9'])]) [Snippet "read ",Argument 1,Snippet " "]])]
--
peggy :: QuasiQuoter
peggy = QuasiQuoter { quoteDec = qDecs, quoteExp = qExp, quotePat = undefined, quoteType = undefined }

-- | Parse peggy syntax from File
-- Parse a peggy syntax file and return a 'Syntax' as a result value.
--
-- > $ cat test.peggy
-- > num :: Int
-- >   = [0-9]+ { return $1 }
--
-- > main = print $(peggyFile "test.peggy")
--
-- The result is:
--
-- > [Definition "num" "Int\n  " (Choice [Semantic (Sequence [Some (TerminalSet [CharRange '0' '9'])]) [Snippet "read ",Argument 1,Snippet " "]])]
--
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

-- | Generates parsers and quasi-quoters.
-- First argument is a list of names of quasi-quoter you want to define.
-- For example:
-- 
-- > genParser [("fooqq", "foo")] [peggy|
-- > foo :: [Int]
-- >   = num*
-- > num ::: Int
-- >   = [0-9]+ { read $1 }
-- > |]
--
-- this code defines parsers named 'foo', 'num' and
-- quasi-quoter named 'fooqq'.
--
-- It can use it as follow:
--
-- > main :: IO ()
-- > main = print [fooqq| 1 2 3 4 5 |]
--
genParser :: [(String, String)] -- ^ a list of pair of name of
                                --   quasi-quoter and its start nonterminal
             -> Syntax          -- ^ syntax
             -> Q [Dec]         -- ^ definitions of parsers and quasi-quoters
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
