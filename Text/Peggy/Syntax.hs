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
  = Terminals Bool Bool String
  | TerminalSet [CharRange]
  | TerminalCmp [CharRange]
  | TerminalAny
  | NonTerminal Identifier
  | Empty
    
  | Named Identifier Expr
  
  | Sequence [Expr]
  | Choice   [Expr]
  | Many     Expr
  | Some     Expr
  | Optional Expr
  | And      Expr
  | Not      Expr
    
  | SepBy  Expr Expr
  | SepBy1 Expr Expr
    
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
