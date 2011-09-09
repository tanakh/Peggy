module Text.Peggy.Parser (
  syntax,
  ) where

import Control.Applicative
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String

import Text.Peggy.Syntax

syntax :: Parser Syntax
syntax = many definition <* skips <* eof

definition :: Parser Definition
definition = Definition <$> identifier <* symbol "=" <*> many expr
  <?> "definition"

expr :: Parser Expr
expr = choiceExpr
  
choiceExpr :: Parser Expr
choiceExpr = sepBy1 sequenceExpr (symbol "/")  >>= \es -> case es of
  [e] -> pure e
  _ -> pure $ Choice es
  <?> "choice expr"

sequenceExpr :: Parser Expr
sequenceExpr = some (try (semanticExpr <* notFollowedBy (symbol "="))) >>= \es -> case es of
  [e] -> pure e
  _ -> pure $ Sequence es

semanticExpr :: Parser Expr
semanticExpr = suffixExpr >>= \e ->
  option e $
    Semantic e <$> (symbol "{" *> codeFragment <* symbol "}")

suffixExpr :: Parser Expr
suffixExpr = prefixExpr >>= go where
  go e = option e $ (symbol "*" *> go (Many e) <|>
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
charLit = noneOf "\""

set :: Parser [CharRange]
set = symbol "[" *> many range <* char ']'

range :: Parser CharRange
range =
  (CharRange <$> rchar <* char '-' <*> rchar) <|>
  (CharOne <$> rchar)
  where
    rchar = noneOf "]"

codeFragment :: Parser CodeFragment
codeFragment = CodeFragment <$> many (noneOf "}")

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
skips = pure () <* many ((pure () <* space) <|> comment)

comment :: Parser ()
comment = lineComment <|> regionComment
  <?> "comment"

lineComment :: Parser ()
lineComment = try (string "--") >> manyTill anyChar (char '\n') >> pure ()

regionComment :: Parser ()
regionComment = try (string "{-") *> com *> string "-}" *> pure () where
  com = many $ regionComment <|> (notFollowedBy (string "-}") *> anyChar *> pure ())
