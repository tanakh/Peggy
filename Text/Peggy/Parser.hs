{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Text.Peggy.Parser (syntax) where
import Control.Applicative
import Data.ListLike.Base hiding (head)
import Data.HashTable.ST.Basic
import Numeric
import Data.Char
import Text.Peggy.Prim
import Text.Peggy.Syntax

data MemoTable_0 str_1 s_2
    = MemoTable_3 {tbl_delimiter :: (HashTable s_2
                                               Int
                                               (Result str_1 ())),
                   tbl_syntax :: (HashTable s_2 Int (Result str_1 Syntax)),
                   tbl_definition :: (HashTable s_2 Int (Result str_1 Definition)),
                   tbl_expr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_choiceExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_semanticExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_sequenceExpr :: (HashTable s_2 Int (Result str_1 Expr)),
                   tbl_namedExpr :: (HashTable s_2 Int (Result str_1 Expr)),
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
                   tbl_haskellType :: (HashTable s_2 Int (Result str_1 TermType)),
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
instance MemoTable (MemoTable_0 str_4)
    where newTable = do t_5 <- new
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
                        t_29 <- new
                        t_30 <- new
                        t_31 <- new
                        return (MemoTable_3 t_5 t_6 t_7 t_8 t_9 t_10 t_11 t_12 t_13 t_14 t_15 t_16 t_17 t_18 t_19 t_20 t_21 t_22 t_23 t_24 t_25 t_26 t_27 t_28 t_29 t_30 t_31)
delimiter :: forall str_32 s_33 . ListLike str_32 Char =>
                                  Parser (MemoTable_0 str_32) str_32 s_33 ()
delimiter = memo tbl_delimiter $ defaultDelimiter
syntax :: forall str_34 s_35 . ListLike str_34 Char =>
                               Parser (MemoTable_0 str_34) str_34 s_35 Syntax
syntax = memo tbl_syntax $ (do v1 <- many definition
                               unexpect (do v1 <- many skip
                                            v2 <- anyChar
                                            return $ (v1, v2))
                               return $ (v1))
definition :: forall str_36 s_37 . ListLike str_36 Char =>
                                   Parser (MemoTable_0 str_36) str_36 s_37 Definition
definition = memo tbl_definition $ token skip delimiter ((do v1 <- ident
                                                             token skip delimiter (string ":::")
                                                             v2 <- haskellType
                                                             token skip delimiter (string "=")
                                                             v3 <- expr
                                                             return (Definition v1 v2 (Token v3))) <|> (do v1 <- ident
                                                                                                           token skip delimiter (string "::")
                                                                                                           v2 <- haskellType
                                                                                                           token skip delimiter (string "=")
                                                                                                           v3 <- expr
                                                                                                           return (Definition v1 v2 v3)))
expr :: forall str_38 s_39 . ListLike str_38 Char =>
                             Parser (MemoTable_0 str_38) str_38 s_39 Expr
expr = memo tbl_expr $ (do v1 <- choiceExpr
                           return $ (v1))
choiceExpr :: forall str_40 s_41 . ListLike str_40 Char =>
                                   Parser (MemoTable_0 str_40) str_40 s_41 Expr
choiceExpr = memo tbl_choiceExpr $ (do v1 <- (do v1 <- do v1 <- semanticExpr
                                                          return $ (v1)
                                                 v2 <- many (do v1 <- do v1 <- do token skip delimiter (string "/")
                                                                                  return $ ()
                                                                         return ()
                                                                v2 <- do v1 <- semanticExpr
                                                                         return $ (v1)
                                                                return v2)
                                                 return (v1 : v2)) <|> (do v1 <- return ()
                                                                           return [])
                                       return (Choice v1))
semanticExpr :: forall str_42 s_43 . ListLike str_42 Char =>
                                     Parser (MemoTable_0 str_42) str_42 s_43 Expr
semanticExpr = memo tbl_semanticExpr $ ((do v1 <- sequenceExpr
                                            token skip delimiter (string "{")
                                            v2 <- codeFragment
                                            token skip delimiter (string "}")
                                            return (Semantic v1 v2)) <|> (do v1 <- sequenceExpr
                                                                             return $ (v1)))
sequenceExpr :: forall str_44 s_45 . ListLike str_44 Char =>
                                     Parser (MemoTable_0 str_44) str_44 s_45 Expr
sequenceExpr = memo tbl_sequenceExpr $ (do v1 <- some (do v1 <- namedExpr
                                                          unexpect (token skip delimiter (string "::"))
                                                          unexpect (token skip delimiter (string "="))
                                                          return $ (v1))
                                           return (Sequence v1))
namedExpr :: forall str_46 s_47 . ListLike str_46 Char =>
                                  Parser (MemoTable_0 str_46) str_46 s_47 Expr
namedExpr = memo tbl_namedExpr $ ((do v1 <- ident
                                      token skip delimiter (string ":")
                                      v2 <- suffixExpr
                                      return (Named v1 v2)) <|> (do v1 <- suffixExpr
                                                                    return $ (v1)))
suffixExpr :: forall str_48 s_49 . ListLike str_48 Char =>
                                   Parser (MemoTable_0 str_48) str_48 s_49 Expr
suffixExpr = memo tbl_suffixExpr $ (do v1 <- do v1 <- prefixExpr
                                                return $ (v1)
                                       v2 <- suffixExpr_tail
                                       return (v2 v1))
suffixExpr_tail :: forall str_50 s_51 . ListLike str_50 Char =>
                                        Parser (MemoTable_0 str_50) str_50 s_51 (Expr -> Expr)
suffixExpr_tail = memo tbl_suffixExpr_tail $ ((((do token skip delimiter (string "*")
                                                    v1 <- suffixExpr_tail
                                                    return (\v999 -> v1 (Many v999))) <|> (do token skip delimiter (string "+")
                                                                                              v1 <- suffixExpr_tail
                                                                                              return (\v999 -> v1 (Some v999)))) <|> (do token skip delimiter (string "?")
                                                                                                                                         v1 <- suffixExpr_tail
                                                                                                                                         return (\v999 -> v1 (Optional v999)))) <|> (do v1 <- return ()
                                                                                                                                                                                        return id))
prefixExpr :: forall str_52 s_53 . ListLike str_52 Char =>
                                   Parser (MemoTable_0 str_52) str_52 s_53 Expr
prefixExpr = memo tbl_prefixExpr $ (((do token skip delimiter (string "&")
                                         v1 <- primExpr
                                         return (And v1)) <|> (do token skip delimiter (string "!")
                                                                  v1 <- primExpr
                                                                  return (Not v1))) <|> (do v1 <- primExpr
                                                                                            return $ (v1)))
primExpr :: forall str_54 s_55 . ListLike str_54 Char =>
                                 Parser (MemoTable_0 str_54) str_54 s_55 Expr
primExpr = memo tbl_primExpr $ token skip delimiter (((((((((do string "\""
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
                                                                                                                                                                                       return (TerminalSet v1))) <|> (do token skip delimiter (string ".")
                                                                                                                                                                                                                         return TerminalAny)) <|> (do v1 <- ident
                                                                                                                                                                                                                                                      return (NonTerminal v1))) <|> (do token skip delimiter (string "(")
                                                                                                                                                                                                                                                                                        v1 <- expr
                                                                                                                                                                                                                                                                                        token skip delimiter (string ",")
                                                                                                                                                                                                                                                                                        v2 <- expr
                                                                                                                                                                                                                                                                                        token skip delimiter (string ")")
                                                                                                                                                                                                                                                                                        return (SepBy v1 v2))) <|> (do token skip delimiter (string "(")
                                                                                                                                                                                                                                                                                                                       v1 <- expr
                                                                                                                                                                                                                                                                                                                       token skip delimiter (string ";")
                                                                                                                                                                                                                                                                                                                       v2 <- expr
                                                                                                                                                                                                                                                                                                                       token skip delimiter (string ")")
                                                                                                                                                                                                                                                                                                                       return (SepBy1 v1 v2))) <|> (do token skip delimiter (string "(")
                                                                                                                                                                                                                                                                                                                                                       v1 <- expr
                                                                                                                                                                                                                                                                                                                                                       token skip delimiter (string ")")
                                                                                                                                                                                                                                                                                                                                                       return $ (v1)))
charLit :: forall str_56 s_57 . ListLike str_56 Char =>
                                Parser (MemoTable_0 str_56) str_56 s_57 Char
charLit = memo tbl_charLit $ ((do string "\\"
                                  v1 <- escChar
                                  return $ (v1)) <|> (do unexpect (satisfy (\c -> (c == '\'') || (c == '"')))
                                                         v1 <- anyChar
                                                         return $ (v1)))
escChar :: forall str_58 s_59 . ListLike str_58 Char =>
                                Parser (MemoTable_0 str_58) str_58 s_59 Char
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
range :: forall str_60 s_61 . ListLike str_60 Char =>
                              Parser (MemoTable_0 str_60) str_60 s_61 CharRange
range = memo tbl_range $ ((do v1 <- rchar
                              string "-"
                              v2 <- rchar
                              return (CharRange v1 v2)) <|> (do v1 <- rchar
                                                                return (CharOne v1)))
rchar :: forall str_62 s_63 . ListLike str_62 Char =>
                              Parser (MemoTable_0 str_62) str_62 s_63 Char
rchar = memo tbl_rchar $ ((((((do string "\\"
                                  v1 <- escChar
                                  return $ (v1)) <|> (do string "\\]"
                                                         return ']')) <|> (do string "\\["
                                                                              return '[')) <|> (do string "\\^"
                                                                                                   return '^')) <|> (do string "\\-"
                                                                                                                        return '-')) <|> (do v1 <- satisfy $ (not . (\c -> c == ']'))
                                                                                                                                             return $ (v1)))
haskellType :: forall str_64 s_65 . ListLike str_64 Char =>
                                    Parser (MemoTable_0 str_64) str_64 s_65 TermType
haskellType = memo tbl_haskellType $ (do v1 <- some (satisfy $ (not . (\c -> c == '=')))
                                         return $ (v1))
codeFragment :: forall str_66 s_67 . ListLike str_66 Char =>
                                     Parser (MemoTable_0 str_66) str_66 s_67 CodeFragment
codeFragment = memo tbl_codeFragment $ (do v1 <- many codePart
                                           return $ (v1))
codePart :: forall str_68 s_69 . ListLike str_68 Char =>
                                 Parser (MemoTable_0 str_68) str_68 s_69 CodePart
codePart = memo tbl_codePart $ ((do v1 <- argument
                                    return $ (v1)) <|> (do v1 <- some (do unexpect (string "}")
                                                                          unexpect argument
                                                                          v1 <- anyChar
                                                                          return $ (v1))
                                                           return (Snippet v1)))
argument :: forall str_70 s_71 . ListLike str_70 Char =>
                                 Parser (MemoTable_0 str_70) str_70 s_71 CodePart
argument = memo tbl_argument $ ((((do string "$$"
                                      v1 <- some digit
                                      return (AntiArgument $ read v1)) <|> (do string "$"
                                                                               v1 <- some digit
                                                                               return (Argument $ read v1))) <|> (do string "$"
                                                                                                                     string "p"
                                                                                                                     return ArgPos)) <|> (do string "$"
                                                                                                                                             string "s"
                                                                                                                                             return ArgSpan))
digit :: forall str_72 s_73 . ListLike str_72 Char =>
                              Parser (MemoTable_0 str_72) str_72 s_73 Char
digit = memo tbl_digit $ (do v1 <- satisfy (\c -> ('0' <= c) && (c <= '9'))
                             return $ (v1))
hexDigit :: forall str_74 s_75 . ListLike str_74 Char =>
                                 Parser (MemoTable_0 str_74) str_74 s_75 Char
hexDigit = memo tbl_hexDigit $ (do v1 <- satisfy (\c -> ((('0' <= c) && (c <= '9')) || (('a' <= c) && (c <= 'f'))) || (('A' <= c) && (c <= 'F')))
                                   return $ (v1))
ident :: forall str_76 s_77 . ListLike str_76 Char =>
                              Parser (MemoTable_0 str_76) str_76 s_77 String
ident = memo tbl_ident $ token skip delimiter (do v1 <- satisfy (\c -> (('a' <= c) && (c <= 'z')) || (c == '_'))
                                                  v2 <- many (satisfy (\c -> (((('0' <= c) && (c <= '9')) || (('a' <= c) && (c <= 'z'))) || (('A' <= c) && (c <= 'Z'))) || (c == '_')))
                                                  return (v1 : v2))
skip :: forall str_78 s_79 . ListLike str_78 Char =>
                             Parser (MemoTable_0 str_78) str_78 s_79 ()
skip = memo tbl_skip $ ((do satisfy (\c -> (((c == ' ') || (c == '\r')) || (c == '\n')) || (c == '\t'))
                            return $ ()) <|> (do v1 <- comment
                                                 return $ (v1)))
comment :: forall str_80 s_81 . ListLike str_80 Char =>
                                Parser (MemoTable_0 str_80) str_80 s_81 ()
comment = memo tbl_comment $ ((do v1 <- lineComment
                                  return $ (v1)) <|> (do v1 <- regionComment
                                                         return $ (v1)))
lineComment :: forall str_82 s_83 . ListLike str_82 Char =>
                                    Parser (MemoTable_0 str_82) str_82 s_83 ()
lineComment = memo tbl_lineComment $ (do string "--"
                                         many (do unexpect (string "\n")
                                                  anyChar
                                                  return $ ())
                                         string "\n"
                                         return $ ())
regionComment :: forall str_84 s_85 . ListLike str_84 Char =>
                                      Parser (MemoTable_0 str_84) str_84 s_85 ()
regionComment = memo tbl_regionComment $ (do string "{-"
                                             many ((do v1 <- regionComment
                                                       return $ (v1)) <|> (do unexpect (string "-}")
                                                                              anyChar
                                                                              return $ ()))
                                             string "-}"
                                             return $ ())
