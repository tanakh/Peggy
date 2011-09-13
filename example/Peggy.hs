{-# Language QuasiQuotes #-}

import Text.Peggy
import Text.Peggy.Syntax
import Text.Peggy.Quote

[peggy|
-- A Parser for peggy itself.

syntax :: Syntax
  = definition* !.

definition :: Definition
  = ident "::" haskellType "=" expr { Definition $1 $2 $3 }

expr :: Expr
  = choiceExpr

choiceExpr :: Expr
  = (semanticExpr, "/") { Choice $1 }

semanticExpr :: Expr
  = sequenceExpr "{" codeFragment "}" { Semantic $1 $2 }
  / sequenceExpr

sequenceExpr :: Expr
  = (suffixExpr !"::" !"=")+ { Sequence $1 }

suffixExpr :: Expr
  = suffixExpr "*" { Many $1 }
  / suffixExpr "+" { Some $1 }
  / suffixExpr "?" { Optional $1 }
  / prefixExpr

prefixExpr :: Expr
  = "&" primExpr { And $1 }
  / "!" primExpr { Not $1 }
  / primExpr

primExpr :: Expr
  = "\"' charLit* '\"" { Terminals   $1 }
  / "[' range* ']"     { TerminalSet $1 }
  / "."                { TerminalAny    }
  / ident              { Nonterminal $1 }
  / "(" expr ")"

charLit :: Char
  = '\\' escChar

escChar :: Char
  = '\n' { '\n' }
  / '\r' { '\r' }
  / '\t' { '\r' }
  / '\\' { '\r' }
  / '\"' { '\r' }
  / '\'' { '\r' }
  / 'x' hexDigit hexDigit { chr . fst . head . readHex $ [$1, $2] }

range :: CharRange
  = [^\]] '-' [^\]] { CharRange $1 $2 }
  / [^\]]           { CharOne $1 }

haskellType :: HaskellType
  = [^=]+

codeFragment :: CodeFragment
  = codePart*

codePart :: CodePart
  = '$' digit+ { Argument $ read $1 }
  / (!'}' .)+  { Snippet $1 }

skip :: ()
  = space / comment

comment :: ()
  = lineComment / regionComment

lineComment :: ()
  = '--' (!'\n' .)* { () }

regionComment :: ()
  = '{-' (regionComment / !'-}' .)* '-}' { () }

digit    :: Char = [0-9] 
hexDigit :: Char = [0-9a-fA-F]
|]

main :: IO ()
main = print . runParser syntax "<stdin>" =<< getContents
