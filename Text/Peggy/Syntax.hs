module Text.Peggy.Syntax (
  Syntax,
  Definition(..),
  Expr(..),
  CharRange(..),
  CodeFragment,
  CodePart(..),
  Identifier,
  HaskellType,
  ) where

type Syntax = [Definition]

data Definition
  = Definition Identifier HaskellType Expr
  deriving (Show)

data Expr
  = Terminals String
  | TerminalSet [CharRange]
  | TerminalAny
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

type CodeFragment = [CodePart]

data CodePart
  = Snippet String
  | Argument Int
  deriving (Show)

type Identifier = String
type HaskellType = String
