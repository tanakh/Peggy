{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}

module Text.Peggy.PrimST (
  Parser(..),
  Result(..),
  ParseError(..),
  MemoTable(..),
  
  memo,
  parse,
  
  anyChar,
  satisfy,
  char,
  string,
  ) where

import Control.Applicative
import Control.Monad.ST
import Control.Monad.Error
import Data.HashTable.ST.Basic as HT
import qualified Data.ListLike as LL

import Text.Peggy.SrcLoc

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
  p <|> q = catchError p (const q)

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

parse :: MemoTable tbl
         => Parser tbl str s a 
         -> str
         -> ST s (Either ParseError a)
parse p str = do
  tbl <- newTable
  res <- unParser p tbl (SrcPos "<input>" 0 1 1) str
  case res of
    Parsed _ _ ret -> return $ Right ret
    Failed err -> return $ Left err

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
char c = satisfy (==c)

string :: LL.ListLike str Char => String -> Parser tbl str s String
string = mapM char
