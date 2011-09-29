-- A Parser for peggy itself.

syntax :: Syntax
  = definition* !(skip* .)

definition ::: Definition
  = ident ":::" haskellType "=" expr { Definition $1 $2 (Token $3) }
  / ident "::"  haskellType "=" expr { Definition $1 $2 $3 }

expr :: Expr
  = choiceExpr

choiceExpr :: Expr
  = (semanticExpr, "/") { Choice $1 }

semanticExpr :: Expr
  = sequenceExpr "{" codeFragment "}" { Semantic $1 $2 }
  / sequenceExpr

sequenceExpr :: Expr
  = (namedExpr !"::" !"=")+ { Sequence $1 }

namedExpr :: Expr
  = ident ":" suffixExpr { Named $1 $2 }
  / suffixExpr

suffixExpr :: Expr
  = suffixExpr "*" { Many $1 }
  / suffixExpr "+" { Some $1 }
  / suffixExpr "?" { Optional $1 }
  / prefixExpr

prefixExpr :: Expr
  = "&" primExpr { And $1 }
  / "!" primExpr { Not $1 }
  / primExpr

primExpr ::: Expr
  = '\"' charLit* '\"'    { Terminals True  True  $1 }
  / '\'' charLit* '\''    { Terminals False False $1 }
  / '[^' range* ']'       { TerminalCmp $1 }
  / '['  range* ']'       { TerminalSet $1 }
  / "."                   { TerminalAny    }
  / ident                 { NonTerminal $1 }
  / "(" expr "," expr ")" { SepBy  $1 $2   }
  / "(" expr ";" expr ")" { SepBy1 $1 $2   }
  / "(" expr ")"

charLit :: Char
  = '\\' escChar
  / ![\'\"] .

escChar :: Char
  = 'n' { '\n' }
  / 'r' { '\r' }
  / 't' { '\t' }
  / '\\' { '\\' }
  / '\"' { '\"' }
  / '\'' { '\'' }
  / 'x' hexDigit hexDigit { chr . fst . head . readHex $ [$1, $2] }

range :: CharRange
  = rchar '-' rchar { CharRange $1 $2 }
  / rchar           { CharOne $1 }

rchar :: Char
  = '\\' escChar
  / '\\]' {']'} / '\\[' { '[' } / '\\^' { '^' } / '\\-' { '-' }
  / [^\]]

haskellType :: TermType
  = [^=]+

codeFragment :: CodeFragment
  = codePart*

codePart :: CodePart
  = argument
  / (!'}' !argument .)+  { Snippet $1 }

argument :: CodePart
  = '$' digit+ { Argument $ read $1 }
  / '$' 'p'    { ArgPos  }
  / '$' 's'    { ArgSpan }

digit    :: Char = [0-9] 
hexDigit :: Char = [0-9a-fA-F]

ident ::: String = [a-z_] [0-9a-zA-Z_]* { $1 : $2 }

skip :: ()
  = _:[ \r\n\t] / comment

comment :: ()
  = lineComment / regionComment

lineComment :: ()
  = '--' _:(!'\n' _:.)* '\n'

regionComment :: ()
  = '{-' _:(regionComment / !'-}' _:.)* '-}'
