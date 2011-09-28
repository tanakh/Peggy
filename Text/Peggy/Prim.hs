{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language RankNTypes #-}

-- |
-- Module      : Text.Peggy.Prim
-- Copyright   : (c) Hideyuki Tanaka 2011
-- License     : BSD-style
--
-- Maintainer  : tanaka.hideyuki@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The monadic parser type and combinators to construct
-- packrat parsers for code generator.
--

module Text.Peggy.Prim (
  -- * Parsing functions
  parse,
  parseString,
  parseFile,
  
  -- * The parser type
  Parser(..),
  -- * The (internal) result type
  Result(..),
  -- * The error type
  ParseError(..),
  -- * The cache type
  MemoTable(..),
  
  -- * Memoising combinator
  memo,
  
  -- * Position functions
  getPos,
  setPos,
  
  -- * Combinators
  anyChar,
  satisfy,
  char,
  string,
  
  expect,
  unexpect,
  
  -- * Utiligy
  space,
  ) where

import Control.Applicative
import Control.Monad.ST
import Control.Monad.Error
import Data.Char
import Data.HashTable.ST.Basic as HT
import qualified Data.ListLike as LL

import Text.Peggy.SrcLoc

-- | Parsing function
parse :: MemoTable tbl
         => (forall s . Parser tbl str s a) -- ^ parser
         -> SrcPos                          -- ^ input information
         -> str                             -- ^ input string
         -> Either ParseError a             -- ^ result
parse p pos str = runST $ do
  tbl <- newTable
  res <- unParser p tbl pos str
  case res of
    Parsed _ _ ret -> return $ Right ret
    Failed err -> return $ Left err

-- | Parsing function with only input name
parseString :: MemoTable tbl
             => (forall s . Parser tbl str s a) -- ^ parser
             -> String                          -- ^ input name
             -> str                             -- ^ input string
             -> Either ParseError a             -- ^ result
parseString p inputName str =
  parse p (SrcPos inputName 0 1 1) str

-- | Parse from file
parseFile :: MemoTable tbl
             => (forall s . Parser tbl String s a) -- ^ parser
             -> FilePath                           -- ^ input filename
             -> IO (Either ParseError a)           -- ^ result
parseFile p fp =
  parse p (SrcPos fp 0 1 1) <$> readFile fp

--

newtype Parser tbl str s a
  = Parser { unParser :: tbl s -> SrcPos -> str -> ST s (Result str a) }

data Result str a
  = Parsed SrcPos str a
  | Failed ParseError

data ParseError
  = ParseError SrcLoc String
  deriving (Show)

instance Error ParseError

nullError :: ParseError
nullError = ParseError (LocPos $ SrcPos "" 0 1 1) ""

errMerge :: ParseError -> ParseError -> ParseError
errMerge e1@(ParseError loc1 msg1) e2@(ParseError loc2 msg2)
  | loc1 >= loc2 = e1
  | otherwise = e2

class MemoTable tbl where
  newTable :: ST s (tbl s)

instance Monad (Parser tbl str s) where
  return v = Parser $ \_ pos s -> return $ Parsed pos s v
  p >>= f = Parser $ \tbl pos s -> do
    res <- unParser p tbl pos s
    case res of
      Parsed qos t x ->
        unParser (f x) tbl qos t
      Failed err ->
        return $ Failed err

instance Functor (Parser tbl str s) where
  fmap f p = return . f =<< p

instance Applicative (Parser tbl str s) where
  pure = return
  p <*> q = do
    f <- p
    x <- q
    return $ f x

instance MonadError ParseError (Parser tbl str s) where
  throwError err = Parser $ \_ _ _ -> return $ Failed err
  catchError p h = Parser $ \tbl pos s -> do
    res <- unParser p tbl pos s
    case res of
      Parsed {} -> return res
      Failed err -> unParser (h err) tbl pos s

instance Alternative (Parser tbl str s) where
  empty = throwError nullError
  p <|> q =
    catchError p $ \perr ->
    catchError q $ \qerr ->
    throwError $ perr `errMerge` qerr

memo :: (tbl s -> HT.HashTable s Int (Result str a))
        -> Parser tbl str s a 
        -> Parser tbl str s a
memo ft p = Parser $ \tbl pos@(SrcPos _ n _ _) s -> do
  cache <- HT.lookup (ft tbl) n
  case cache of
    Just v -> return v
    Nothing -> do
      v <- unParser p tbl pos s
      HT.insert (ft tbl) n v
      return v

getPos :: Parser tbl str s SrcPos
getPos = Parser $ \_ pos str -> return $ Parsed pos str pos

setPos :: SrcPos -> Parser tbl str s ()
setPos pos = Parser $ \_ _ str -> return $ Parsed pos str ()

parseError :: String -> Parser tbl str s a
parseError msg =
  throwError =<< ParseError . LocPos <$> getPos <*> pure msg

anyChar :: LL.ListLike str Char => Parser tbl str s Char
anyChar = Parser $ \_ pos str ->
  if LL.null str
  then return $ Failed nullError
  else do
    let c  = LL.head str
        cs = LL.tail str
    return $ Parsed (pos `advance` c) cs c

satisfy :: LL.ListLike str Char => (Char -> Bool) -> Parser tbl str s Char
satisfy p = do
  c <- anyChar
  when (not $ p c) $ throwError nullError
  return c

char :: LL.ListLike str Char => Char -> Parser tbl str s Char
char c = satisfy (==c) <|> parseError ("expect " ++ show c)

string :: LL.ListLike str Char => String -> Parser tbl str s String
string str = mapM char str <|> parseError ("expect " ++ show str)

expect :: LL.ListLike str Char => Parser tbl str s a -> Parser tbl str s ()
expect p = do
  b <- test p
  when (not b) $ parseError "unexpected input"

unexpect :: LL.ListLike str Char => Parser tbl str s a -> Parser tbl str s ()
unexpect p = do
  b <- test p
  when b $ parseError "unexpected input"

test :: LL.ListLike str Char => Parser tbl str s a -> Parser tbl str s Bool
test p = Parser $ \tbl pos str -> do
  res <- unParser p tbl pos str
  return $ case res of
    Parsed _ _ _ -> Parsed pos str True
    Failed _ -> Parsed pos str False

space :: LL.ListLike str Char => Parser tbl str s ()
space = () <$ satisfy isSpace
