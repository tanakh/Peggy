{-# LANGUAGE FlexibleContexts #-}

import Text.Peggy.PrimST

import Control.Applicative
import Control.Monad.ST
import qualified Data.ByteString as B
import Data.Char
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.ListLike as LL

data ExprMemoTable str s
  = ExprMemoTable
    { tbl_expr :: HT.HashTable s Int (Result str Double)
    , tbl_term :: HT.HashTable s Int (Result str Double)
    , tbl_fact :: HT.HashTable s Int (Result str Double)
    }

instance MemoTable (ExprMemoTable str) where
  newTable = do
    v1 <- HT.new
    v2 <- HT.new
    v3 <- HT.new
    return $ ExprMemoTable v1 v2 v3

expr :: LL.ListLike str Char => Parser (ExprMemoTable str) str s Double
expr = memo tbl_expr $ (+) <$> term <* string "+" <*> expr <|> term

term :: LL.ListLike str Char => Parser (ExprMemoTable str) str s Double
term = memo tbl_term $ (*) <$> fact <* string "*" <*> term <|> fact

fact :: LL.ListLike str Char => Parser (ExprMemoTable str) str s Double
fact = memo tbl_fact $ read <$> some (satisfy isDigit)

-----

main :: IO ()
main = do
  con <- B.getLine
  print $ runST $ parse expr $ LL.CS con
