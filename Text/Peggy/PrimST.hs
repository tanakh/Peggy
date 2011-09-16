{-# Language MultiParamTypeClasses #-}

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

import Text.Peggy.SrcLoc

newtype Parser t s a
  = Parser { unParser :: t s -> SrcPos -> String -> ST s (Result a) }

data Result a
  = Parsed SrcPos String a
  | Failed ParseError

data ParseError
  = ParseError SrcLoc String
  deriving (Show)

instance Error ParseError

nullError :: ParseError
nullError = ParseError (LocPos $ SrcPos "" 0 1 1) ""

class MemoTable t where
  newTable :: ST s (t s)

instance Monad (Parser t s) where
  return v = Parser $ \_ pos s -> return $ Parsed pos s v
  p >>= f = Parser $ \tbl pos s -> do
    res <- unParser p tbl pos s
    case res of
      Parsed qos t x ->
        unParser (f x) tbl qos t
      Failed err ->
        return $ Failed err

instance Functor (Parser t s) where
  fmap f p = return . f =<< p

instance Applicative (Parser t s) where
  pure = return
  p <*> q = do
    f <- p
    x <- q
    return $ f x

instance MonadError ParseError (Parser t s) where
  throwError err = Parser $ \_ _ _ -> return $ Failed err
  catchError p h = Parser $ \tbl pos s -> do
    res <- unParser p tbl pos s
    case res of
      Parsed {} -> return res
      Failed err -> unParser (h err) tbl pos s

instance Alternative (Parser t s) where
  empty = throwError nullError
  p <|> q = catchError p (const q)

memo :: (t s -> HT.HashTable s Int (Result a)) -> Parser t s a -> Parser t s a
memo ft p = Parser $ \tbl pos@(SrcPos _ n _ _) s -> do
  cache <- HT.lookup (ft tbl) n
  case cache of
    Just v -> return v
    Nothing -> do
      v <- unParser p tbl pos s
      HT.insert (ft tbl) n v
      return v

parse :: MemoTable t => Parser t s a -> String -> ST s (Either ParseError a)
parse p str = do
  tbl <- newTable
  res <- unParser p tbl (SrcPos "<input>" 0 1 1) str
  case res of
    Parsed _ _ ret -> return $ Right ret
    Failed err -> return $ Left err

anyChar :: Parser t s Char
anyChar = Parser $ \_ pos str -> case str of
  (c:cs) -> return $ Parsed (pos `advance` c) cs c
  _ -> return $ Failed nullError

satisfy :: (Char -> Bool) -> Parser t s Char
satisfy p = do
  c <- anyChar
  when (not $ p c) $ throwError nullError
  return c

char :: Char -> Parser t s Char
char c = satisfy (==c)

string :: String -> Parser t s String
string = mapM char
