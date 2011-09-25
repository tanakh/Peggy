{-# Language RankNTypes #-}
{-# Language FlexibleContexts #-}
module Text.Peggy.Parser (syntax) where
import Control.Applicative
import Data.ListLike.Base hiding (head)
import Data.HashTable.ST.Basic
import Numeric
import Data.Char
import Text.Peggy.Prim
import Text.Peggy.Syntax

data MemoTable_0 str_1 s_2
    = MemoTable_0 {tbl_syntax :: (HashTable s_2
                                            Int
                                            (Result str_1 Syntax)),
                   tbl_definition :: (HashTable s_2 Int (Result str_1 Definition)),
                   tbl_expr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_choiceExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_semanticExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_sequenceExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_suffixExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_suffixExpr_tail :: (HashTable s_2
                                                     Int
                                                     (Result str_1 (Expr -> Expr))),
                   tbl_prefixExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_primExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_charLit :: (HashTable s_2 Int (Result str_1 Char)),
                   tbl_escChar :: (HashTable s_2 Int (Result str_1 Char)),
                   tbl_range :: (HashTable s_2 Int (Result str_1 CharRange)),
                   tbl_rchar :: (HashTable s_2 Int (Result str_1 Char)),
                   tbl_haskellType :: (HashTable s_2 Int (Result str_1 HaskellType)),
                   tbl_codeFragment :: (HashTable s_2
                                                  Int
                                                  (Result str_1 CodeFragment)),
                   tbl_codePart :: (HashTable s_2 Int (Result str_1 CodePart)),
                   tbl_argument :: (HashTable s_2 Int (Result str_1 CodePart)),
                   tbl_digit :: (HashTable s_2 Int (Result str_1 Char)),
                   tbl_hexDigit :: (HashTable s_2 Int (Result str_1 Char)),
                   tbl_ident :: (HashTable s_2 Int (Result str_1 String)),
                   tbl_skip :: (HashTable s_2 Int (Result str_1 ())),
                   tbl_comment :: (HashTable s_2 Int (Result str_1 ())),
                   tbl_lineComment :: (HashTable s_2 Int (Result str_1 ())),
                   tbl_regionComment :: (HashTable s_2 Int (Result str_1 ()))}
instance MemoTable (MemoTable_0 str_3)
    where newTable = do t_4 <- new
                        t_5 <- new
                        t_6 <- new
                        t_7 <- new
                        t_8 <- new
                        t_9 <- new
                        t_10 <- new
                        t_11 <- new
                        t_12 <- new
                        t_13 <- new
                        t_14 <- new
                        t_15 <- new
                        t_16 <- new
                        t_17 <- new
                        t_18 <- new
                        t_19 <- new
                        t_20 <- new
                        t_21 <- new
                        t_22 <- new
                        t_23 <- new
                        t_24 <- new
                        t_25 <- new
                        t_26 <- new
                        t_27 <- new
                        t_28 <- new
                        return (MemoTable_0 t_4 t_5 t_6 t_7 t_8 t_9 t_10 t_11 t_12 t_13 t_14 t_15 t_16 t_17 t_18 t_19 t_20 t_21 t_22 t_23 t_24 t_25 t_26 t_27 t_28)
syntax :: forall str_29 s_30 . ListLike str_29 Char =>
                               Parser (MemoTable_0 str_29) str_29 s_30 Syntax
syntax = memo tbl_syntax $ (do v1 <- many definition
                               unexpect (do v1 <- many skip
                                            v2 <- anyChar
                                            return $ (v1, v2))
                               return $ (v1))
definition :: forall str_31 s_32 . ListLike str_31 Char =>
                                   Parser (MemoTable_0 str_31) str_31 s_32 Definition
definition = memo tbl_definition $ ((many skip *> ((do v1 <- ident
                                                       (many skip *> string ":::") <* many skip
                                                       v2 <- haskellType
                                                       (many skip *> string "=") <* many skip
                                                       v3 <- expr
                                                       return (Definition v1 v2 (Token v3))) <|> (do v1 <- ident
                                                                                                     (many skip *> string "::") <* many skip
                                                                                                     v2 <- haskellType
                                                                                                     (many skip *> string "=") <* many skip
                                                                                                     v3 <- expr
                                                                                                     return (Definition v1 v2 v3)))) <* many skip)
expr :: forall str_33 s_34 . ListLike str_33 Char =>
                             Parser (MemoTable_0 str_33) str_33 s_34 Expr
expr = memo tbl_expr $ (do v1 <- choiceExpr
                           return $ (v1))
choiceExpr :: forall str_35 s_36 . ListLike str_35 Char =>
                                   Parser (MemoTable_0 str_35) str_35 s_36 Expr
choiceExpr = memo tbl_choiceExpr $ (do v1 <- (do v1 <- do v1 <- semanticExpr
                                                          return $ (v1)
                                                 v2 <- many (do v1 <- do v1 <- do (many skip *> string "/") <* many skip
                                                                                  return $ ()
                                                                         return ()
                                                                v2 <- do v1 <- semanticExpr
                                                                         return $ (v1)
                                                                return v2)
                                                 return (v1 : v2)) <|> (do v1 <- return ()
                                                                           return [])
                                       return (Choice v1))
semanticExpr :: forall str_37 s_38 . ListLike str_37 Char =>
                                     Parser (MemoTable_0 str_37) str_37 s_38 Expr
semanticExpr = memo tbl_semanticExpr $ ((do v1 <- sequenceExpr
                                            (many skip *> string "{") <* many skip
                                            v2 <- codeFragment
                                            (many skip *> string "}") <* many skip
                                            return (Semantic v1 v2)) <|> (do v1 <- sequenceExpr
                                                                             return $ (v1)))
sequenceExpr :: forall str_39 s_40 . ListLike str_39 Char =>
                                     Parser (MemoTable_0 str_39) str_39 s_40 Expr
sequenceExpr = memo tbl_sequenceExpr $ (do v1 <- some (do v1 <- suffixExpr
                                                          unexpect ((many skip *> string "::") <* many skip)
                                                          unexpect ((many skip *> string "=") <* many skip)
                                                          return $ (v1))
                                           return (Sequence v1))
suffixExpr :: forall str_41 s_42 . ListLike str_41 Char =>
                                   Parser (MemoTable_0 str_41) str_41 s_42 Expr
suffixExpr = memo tbl_suffixExpr $ (do v1 <- do v1 <- prefixExpr
                                                return $ (v1)
                                       v2 <- suffixExpr_tail
                                       return (v2 v1))
suffixExpr_tail :: forall str_43 s_44 . ListLike str_43 Char =>
                                        Parser (MemoTable_0 str_43) str_43 s_44 (Expr -> Expr)
suffixExpr_tail = memo tbl_suffixExpr_tail $ ((((do (many skip *> string "*") <* many skip
                                                    v1 <- suffixExpr_tail
                                                    return (\v999 -> v1 (Many v999))) <|> (do (many skip *> string "+") <* many skip
                                                                                              v1 <- suffixExpr_tail
                                                                                              return (\v999 -> v1 (Some v999)))) <|> (do (many skip *> string "?") <* many skip
                                                                                                                                         v1 <- suffixExpr_tail
                                                                                                                                         return (\v999 -> v1 (Optional v999)))) <|> (do v1 <- return ()
                                                                                                                                                                                        return id))
prefixExpr :: forall str_45 s_46 . ListLike str_45 Char =>
                                   Parser (MemoTable_0 str_45) str_45 s_46 Expr
prefixExpr = memo tbl_prefixExpr $ (((do (many skip *> string "&") <* many skip
                                         v1 <- primExpr
                                         return (And v1)) <|> (do (many skip *> string "!") <* many skip
                                                                  v1 <- primExpr
                                                                  return (Not v1))) <|> (do v1 <- primExpr
                                                                                            return $ (v1)))
primExpr :: forall str_47 s_48 . ListLike str_47 Char =>
                                 Parser (MemoTable_0 str_47) str_47 s_48 Expr
primExpr = memo tbl_primExpr $ ((many skip *> (((((((((do string "\""
                                                          v1 <- many charLit
                                                          string "\""
                                                          return (Terminals True True v1)) <|> (do string "'"
                                                                                                   v1 <- many charLit
                                                                                                   string "'"
                                                                                                   return (Terminals False False v1))) <|> (do string "[^"
                                                                                                                                               v1 <- many range
                                                                                                                                               string "]"
                                                                                                                                               return (TerminalCmp v1))) <|> (do string "["
                                                                                                                                                                                 v1 <- many range
                                                                                                                                                                                 string "]"
                                                                                                                                                                                 return (TerminalSet v1))) <|> (do (many skip *> string ".") <* many skip
                                                                                                                                                                                                                   return TerminalAny)) <|> (do v1 <- ident
                                                                                                                                                                                                                                                return (NonTerminal v1))) <|> (do (many skip *> string "(") <* many skip
                                                                                                                                                                                                                                                                                  v1 <- expr
                                                                                                                                                                                                                                                                                  (many skip *> string ",") <* many skip
                                                                                                                                                                                                                                                                                  v2 <- expr
                                                                                                                                                                                                                                                                                  (many skip *> string ")") <* many skip
                                                                                                                                                                                                                                                                                  return (SepBy v1 v2))) <|> (do (many skip *> string "(") <* many skip
                                                                                                                                                                                                                                                                                                                 v1 <- expr
                                                                                                                                                                                                                                                                                                                 (many skip *> string ";") <* many skip
                                                                                                                                                                                                                                                                                                                 v2 <- expr
                                                                                                                                                                                                                                                                                                                 (many skip *> string ")") <* many skip
                                                                                                                                                                                                                                                                                                                 return (SepBy1 v1 v2))) <|> (do (many skip *> string "(") <* many skip
                                                                                                                                                                                                                                                                                                                                                 v1 <- expr
                                                                                                                                                                                                                                                                                                                                                 (many skip *> string ")") <* many skip
                                                                                                                                                                                                                                                                                                                                                 return $ (v1)))) <* many skip)
charLit :: forall str_49 s_50 . ListLike str_49 Char =>
                                Parser (MemoTable_0 str_49) str_49 s_50 Char
charLit = memo tbl_charLit $ ((do string "\\"
                                  v1 <- escChar
                                  return $ (v1)) <|> (do unexpect (satisfy (\c -> (c == '\'') || (c == '"')))
                                                         v1 <- anyChar
                                                         return $ (v1)))
escChar :: forall str_51 s_52 . ListLike str_51 Char =>
                                Parser (MemoTable_0 str_51) str_51 s_52 Char
escChar = memo tbl_escChar $ (((((((do string "n"
                                       return '\n') <|> (do string "r"
                                                            return '\r')) <|> (do string "t"
                                                                                  return '\t')) <|> (do string "\\"
                                                                                                        return '\\')) <|> (do string "\""
                                                                                                                              return '"')) <|> (do string "'"
                                                                                                                                                   return '\'')) <|> (do string "x"
                                                                                                                                                                         v1 <- hexDigit
                                                                                                                                                                         v2 <- hexDigit
                                                                                                                                                                         return ((chr . (fst . (head . readHex))) $ [v1,
                                                                                                                                                                                                                     v2])))
range :: forall str_53 s_54 . ListLike str_53 Char =>
                              Parser (MemoTable_0 str_53) str_53 s_54 CharRange
range = memo tbl_range $ ((do v1 <- rchar
                              string "-"
                              v2 <- rchar
                              return (CharRange v1 v2)) <|> (do v1 <- rchar
                                                                return (CharOne v1)))
rchar :: forall str_55 s_56 . ListLike str_55 Char =>
                              Parser (MemoTable_0 str_55) str_55 s_56 Char
rchar = memo tbl_rchar $ ((((((do string "\\"
                                  v1 <- escChar
                                  return $ (v1)) <|> (do string "\\]"
                                                         return ']')) <|> (do string "\\["
                                                                              return '[')) <|> (do string "\\^"
                                                                                                   return '^')) <|> (do string "\\-"
                                                                                                                        return '-')) <|> (do v1 <- satisfy $ (not . (\c -> c == ']'))
                                                                                                                                             return $ (v1)))
haskellType :: forall str_57 s_58 . ListLike str_57 Char =>
                                    Parser (MemoTable_0 str_57) str_57 s_58 HaskellType
haskellType = memo tbl_haskellType $ (do v1 <- some (satisfy $ (not . (\c -> c == '=')))
                                         return $ (v1))
codeFragment :: forall str_59 s_60 . ListLike str_59 Char =>
                                     Parser (MemoTable_0 str_59) str_59 s_60 CodeFragment
codeFragment = memo tbl_codeFragment $ (do v1 <- many codePart
                                           return $ (v1))
codePart :: forall str_61 s_62 . ListLike str_61 Char =>
                                 Parser (MemoTable_0 str_61) str_61 s_62 CodePart
codePart = memo tbl_codePart $ ((do v1 <- argument
                                    return $ (v1)) <|> (do v1 <- some (do unexpect (string "}")
                                                                          unexpect argument
                                                                          v1 <- anyChar
                                                                          return $ (v1))
                                                           return (Snippet v1)))
argument :: forall str_63 s_64 . ListLike str_63 Char =>
                                 Parser (MemoTable_0 str_63) str_63 s_64 CodePart
argument = memo tbl_argument $ (do string "$"
                                   v1 <- some digit
                                   return (Argument $ read v1))
digit :: forall str_65 s_66 . ListLike str_65 Char =>
                              Parser (MemoTable_0 str_65) str_65 s_66 Char
digit = memo tbl_digit $ (do v1 <- satisfy (\c -> ('0' <= c) && (c <= '9'))
                             return $ (v1))
hexDigit :: forall str_67 s_68 . ListLike str_67 Char =>
                                 Parser (MemoTable_0 str_67) str_67 s_68 Char
hexDigit = memo tbl_hexDigit $ (do v1 <- satisfy (\c -> ((('0' <= c) && (c <= '9')) || (('a' <= c) && (c <= 'f'))) || (('A' <= c) && (c <= 'F')))
                                   return $ (v1))
ident :: forall str_69 s_70 . ListLike str_69 Char =>
                              Parser (MemoTable_0 str_69) str_69 s_70 String
ident = memo tbl_ident $ ((many skip *> (do v1 <- satisfy (\c -> ((('a' <= c) && (c <= 'z')) || (('A' <= c) && (c <= 'Z'))) || (c == '_'))
                                            v2 <- many (satisfy (\c -> (((('0' <= c) && (c <= '9')) || (('a' <= c) && (c <= 'z'))) || (('A' <= c) && (c <= 'Z'))) || (c == '_')))
                                            return (v1 : v2))) <* many skip)
skip :: forall str_71 s_72 . ListLike str_71 Char =>
                             Parser (MemoTable_0 str_71) str_71 s_72 ()
skip = memo tbl_skip $ ((do v1 <- satisfy (\c -> (((c == ' ') || (c == '\r')) || (c == '\n')) || (c == '\t'))
                            return ()) <|> (do v1 <- comment
                                               return $ (v1)))
comment :: forall str_73 s_74 . ListLike str_73 Char =>
                                Parser (MemoTable_0 str_73) str_73 s_74 ()
comment = memo tbl_comment $ ((do v1 <- lineComment
                                  return $ (v1)) <|> (do v1 <- regionComment
                                                         return $ (v1)))
lineComment :: forall str_75 s_76 . ListLike str_75 Char =>
                                    Parser (MemoTable_0 str_75) str_75 s_76 ()
lineComment = memo tbl_lineComment $ (do string "--"
                                         v1 <- many (do unexpect (string "\n")
                                                        v1 <- anyChar
                                                        return $ (v1))
                                         string "\n"
                                         return ())
regionComment :: forall str_77 s_78 . ListLike str_77 Char =>
                                      Parser (MemoTable_0 str_77) str_77 s_78 ()
regionComment = memo tbl_regionComment $ (do string "{-"
                                             v1 <- many ((do v1 <- regionComment
                                                             return $ (v1)) <|> (do unexpect (string "-}")
                                                                                    v1 <- anyChar
                                                                                    return ()))
                                             string "-}"
                                             return ())
