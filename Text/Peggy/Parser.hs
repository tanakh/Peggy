module Text.Peggy.Parser (
  syntax,
  ) where

import Control.Applicative
import Data.Char
import Numeric
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String

import Text.Peggy.Syntax

syntax :: Parser Syntax
syntax = many definition <* skips <* eof

definition :: Parser Definition
definition =
  Definition <$> identifier <* symbol ":" <*> haskellType <* symbol "=" <*> expr
  <?> "definition"

expr :: Parser Expr
expr = choiceExpr
  
choiceExpr :: Parser Expr
choiceExpr = sepBy1 semanticExpr (symbol "/")  >>= \es -> case es of
  [e] -> pure e
  _ -> pure $ Choice es
  <?> "choice expr"

semanticExpr :: Parser Expr
semanticExpr = sequenceExpr >>= \e ->
  option e $
    Semantic e <$> (symbol "{" *> codeFragment <* symbol "}")

sequenceExpr :: Parser Expr
sequenceExpr = some (try (suffixExpr <* notFollowedBy (symbol ":" <|> symbol "="))) >>= \es -> case es of
  [e] -> pure e
  _ -> pure $ Sequence es

suffixExpr :: Parser Expr
suffixExpr = prefixExpr >>= go where
  go e = option e (symbol "*" *> go (Many e) <|>
                   symbol "+" *> go (Some e) <|>
                   symbol "?" *> go (Optional e))

prefixExpr :: Parser Expr
prefixExpr =
  (And <$ symbol "?" <*> primExpr) <|>
  (Not <$ symbol "!" <*> primExpr) <|>
  primExpr

primExpr :: Parser Expr
primExpr =
  (Terminals <$> stringLit) <|>
  (TerminalSet <$> set) <|>
  (NonTerminal <$> identifier) <|>
  symbol "(" *> expr <* symbol ")"
  <?> "primitive expression"

stringLit :: Parser String
stringLit = lexeme (char '\"' *> many charLit <* char '\"')
  <?> "literal"

charLit :: Parser Char
charLit = escaped <|> noneOf "\"" where
  escaped = char '\\' >> escChar
  
  escChar =
    ('\n' <$ char 'n' ) <|>
    ('\r' <$ char 'r' ) <|>
    ('\t' <$ char 't' ) <|>
    ('\\' <$ char '\\') <|>
    ('\"' <$ char '\"') <|>
    ('\'' <$ char '\'') <|>
    (chr . fst . head . readHex <$ char 'x' <*> count 2 hexDigit)

set :: Parser [CharRange]
set = symbol "[" *> many range <* char ']'

range :: Parser CharRange
range =
  try (CharRange <$> rchar <* char '-' <*> rchar) <|>
  (CharOne <$> rchar)
  where
    rchar = noneOf "]"

haskellType :: Parser HaskellType
haskellType = some (noneOf "=")
  <?> "type signature"

codeFragment :: Parser CodeFragment
codeFragment = many codePart
  <?> "code fragment"

codePart :: Parser CodePart
codePart =
  try argument <|>
  Snippet <$> some (try (notFollowedBy argument >> noneOf "}"))

argument :: Parser CodePart
argument = try $ Argument <$ char '$' <*> number where
  number = read <$> some digit

--

identifier :: Parser String
identifier =
  lexeme ((:) <$> startChar <*> many subsequentChar)
  <?> "identifier"  
  where
    startChar = char '_' <|> letter
    subsequentChar = startChar <|> digit

symbol :: String -> Parser String
symbol s = lexeme (string s)
  <?> "symbol: " ++ s

lexeme :: Parser a -> Parser a
lexeme p = try $ skips *> p

skips :: Parser ()
skips = () <$ many ((() <$ space) <|> comment)

comment :: Parser ()
comment = lineComment <|> regionComment
  <?> "comment"

lineComment :: Parser ()
lineComment = () <$ try (string "--") <* manyTill anyChar (char '\n')

regionComment :: Parser ()
regionComment = () <$ try (string "{-") <* com <* string "-}" where
  com = () <$ many (regionComment <|> (notFollowedBy (string "-}") <* anyChar))
