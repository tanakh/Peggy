{-# Language GeneralizedNewtypeDeriving #-}

module Text.Peggy (
  ParserT(..),
  runParser,
  
  satisfy, satisfyChar,
  
  string,
  empty,
  (<|>),
  many,
  some,
  optional,
  expect,
  unexpect,
  ) where

import Control.Applicative
import Control.Exception.Control
import Control.Monad.IO.Control
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe

newtype ParserT m a =
  ParserT { unParserT :: ErrorT String (StateT ParserState m) a }
  deriving (Functor, Applicative,
            Monad, MonadIO, MonadControlIO,
            MonadState ParserState,
            MonadError String)

-- alternative semantics is back track
instance (Applicative m, Monad m) => Alternative (ParserT m) where
  empty = throwError ""
  
  a <|> b = do
    bef <- get
    a `catchError` (\_ -> put bef >> b)

instance (Applicative m, Monad m) => MonadPlus (ParserT m) where
  mzero = empty
  mplus = (<|>)

data ParserState =
  ParserState
  { psPos :: !Posn
  , psCur :: String
  }

data Posn = Posn !Int !Int !Int -- absp line col

runParser :: Monad m => ParserT m a -> String -> m (Either String a)
runParser p str = do
  evalStateT (runErrorT $ unParserT p) (ParserState (Posn 0 1 1) str)

tabWidth :: Int
tabWidth = 8

movePosn :: Posn -> Char -> Posn
movePosn (Posn absp line col) c =
  case c of
    '\t' -> Posn (absp + 1) line ((col - 1 + tabWidth - 1) `div` tabWidth * tabWidth + 1)
    '\n' -> Posn (absp + 1) (line + 1) 1
    _    -> Posn (absp + 1) line (col + 1)

move :: Monad m => ParserT m ()
move = do
  s <- get
  let c = head $ psCur s
  put $ s { psPos = psPos s `movePosn` c, psCur = tail $ psCur s}

satisfy :: (Applicative m, MonadPlus m) => Int -> (String -> Bool) -> ParserT m String
satisfy n p = do
  s <- take n <$> gets psCur
  guard $ p s
  replicateM_ n move
  return s

satisfyChar :: (Applicative m, MonadPlus m) => (Char -> Bool) -> ParserT m Char
satisfyChar p = head <$> satisfy 1 (\cs -> p $ head cs)

string :: (Applicative m, MonadPlus m) => String -> ParserT m String
string str = satisfy (length str) (== str)

expect :: MonadControlIO m => ParserT m a -> ParserT m ()
expect p = btrack $ p >> return ()

unexpect :: (Applicative m, MonadControlIO m) => ParserT m a -> ParserT m ()
unexpect p = btrack $ do
  b <- optional p
  if isJust b then empty else pure ()

btrack :: MonadControlIO m => ParserT m a -> ParserT m a
btrack p = bracket get put $ const p
