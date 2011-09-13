-- A Parser for peggy itself.

syntax :: Syntax
  = definition* !(skip* .)

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
  = "' [\"\'] charLit* [\"\'] '" { Terminals ($1=='\"') ($3=='\"') $2 }
  / "[^' range* ']"              { TerminalCmp $1 }
  / "['  range* ']"              { TerminalSet $1 }
  / "."                          { TerminalAny    }
  / ident                        { NonTerminal $1 }
  / "(" expr "," expr ")"        { SepBy  $1 $2   }
  / "(" expr ";" expr ")"        { SepBy1 $1 $2   }
  / "(" expr ")"

charLit :: Char
  = '\\' escChar
  / ![\'\"] .

escChar :: Char
  = '\n' { '\n' }
  / '\r' { '\r' }
  / '\t' { '\r' }
  / '\\' { '\r' }
  / '\"' { '\r' }
  / '\'' { '\r' }
  / 'x' hexDigit hexDigit { chr . fst . head . readHex $ [$1, $2] }

range :: CharRange
  = rchar '-' rchar { CharRange $1 $2 }
  / rchar           { CharOne $1 }

rchar :: Char
  = '\\' escChar
  / '\\]' {']'} / '\\[' { '[' } / '\\^' { '^' } / '\\-' { '-' }
  / [^\]]

haskellType :: HaskellType
  = [^=]+

codeFragment :: CodeFragment
  = codePart*

codePart :: CodePart
  = argument
  / (!'}' !argument .)+  { Snippet $1 }

argument :: CodePart
  = '$' digit+ { Argument $ read $1 }

digit    :: Char = [0-9] 
hexDigit :: Char = [0-9a-fA-F]

ident :: String = "' [a-zA-Z_] [0-9a-zA-Z_]* '" { $1 : $2 }

skip :: ()
  = [ \r\n\t] { () } / comment

comment :: ()
  = lineComment / regionComment

lineComment :: ()
  = '--' (!'\n' .)* '\n' { () }

regionComment :: ()
  = '{-' (regionComment / !'-}' . { () } )* '-}' { () }
