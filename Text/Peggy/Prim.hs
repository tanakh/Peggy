{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}

module Text.Peggy.Prim (
  Parser(..),
  ParseError(..),
  Result(..),
  Derivs(..),
  runParser,
  
  getPos,
  
  anyChar,
  satisfy,
  char,
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
import Control.Monad.State
import Control.Monad.Error

import Text.Peggy.SrcLoc

newtype Parser d a = Parser { unParser :: d -> Result d a }

data ParseError = ParseError SrcLoc String
  deriving (Show)

nullError :: ParseError
nullError = ParseError (LocPos (SrcPos "" 0 1 1)) ""

data Result d a
  = Parsed d a
  | Failed ParseError

instance Show a => Show (Result d a) where
  show (Parsed _ a) = "Parsed " ++ show a
  show (Failed err) = "Failed (" ++ show err ++ ")"

class Derivs d where
  dvPos  :: d -> SrcPos
  dvChar :: d -> Result d Char
  parse  :: SrcPos -> String -> d

runParser :: Derivs d => Parser d a -> String -> String -> Result d a
runParser p sourceName source =
  unParser p $ parse (SrcPos sourceName 0 1 1) source

instance Functor (Parser d) where
  fmap f (Parser p) = Parser $ \d ->
    case p d of
      Parsed e a ->
        Parsed e (f a)
      Failed err ->
        Failed err

instance Applicative (Parser d) where
  pure a = Parser $ \d -> Parsed d a
  
  Parser p <*> Parser q = Parser $ \d ->
    case p d of
      Parsed e g ->
        case q e of
          Parsed f a ->
            Parsed f (g a)
          Failed err ->
            Failed err
      Failed err ->
        Failed err

instance Monad (Parser d) where
  return a = Parser $ \d -> Parsed d a
  
  Parser p >>= f = Parser $ \d -> 
    case p d of
      Parsed e a ->
        unParser (f a) e
      Failed err ->
        Failed err

instance Derivs d => Alternative (Parser d) where
  empty =
    Parser $ \d -> Failed (ParseError (LocPos $ dvPos d) "")
  Parser p <|> Parser q =
    Parser $ \d -> 
    case p d of
      Parsed e a ->
        Parsed e a
      Failed _ ->
        q d

instance MonadError ParseError (Parser d) where
  throwError e = Parser $ \_ ->
    Failed e
  catchError (Parser p) h = Parser $ \d ->
    case p d of
      Parsed e a ->
        Parsed e a
      Failed err ->
        unParser (h err) d

--

backtrack :: Derivs d => Parser d a -> Parser d a
backtrack (Parser p) = Parser $ \d ->
  case p d of
    Parsed _ r ->
      Parsed d r
    Failed e ->
      Failed e

getPos :: Derivs d => Parser d SrcPos
getPos = Parser $ \d ->
  Parsed d (dvPos d)

anyChar :: Derivs d => Parser d Char
anyChar = Parser dvChar

satisfy :: Derivs d => (Char -> Bool) -> Parser d Char
satisfy p = do
  c <- anyChar
  when (not $ p c) $
    throwError nullError
  return c

char :: Derivs d => Char -> Parser d Char
char c = satisfy (== c)

string :: Derivs d => String -> Parser d String
string = mapM char

expect :: Derivs d => Parser d a -> Parser d ()
expect p = backtrack $ () <$ p

unexpect :: Derivs d => Parser d a -> Parser d ()
unexpect p = backtrack $ do
  b <- catchError (True <$ p) (\_ -> pure False)
  when b $ throwError nullError
