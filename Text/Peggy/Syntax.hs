module Text.Peggy.Syntax (
  Syntax,
  Definition(..),
  Expr(..),
  CharRange(..),
  CodeFragment(..),
  Identifier,
  ) where

type Syntax = [Definition]

data Definition
  = Definition Identifier [Expr]
  deriving (Show)

data Expr
  = Terminals String
  | TerminalSet [CharRange]
  | NonTerminal Identifier
  | Empty
  
  | Sequence [Expr]
  | Choice   [Expr]
  | Many     Expr
  | Some     Expr
  | Optional Expr
  | And      Expr
  | Not      Expr
    
  | Semantic Expr CodeFragment
  deriving (Show)

data CharRange
  = CharRange Char Char
  | CharOne Char
  deriving (Show)

data CodeFragment
  = CodeFragment String
  deriving (Show)

type Identifier = String
