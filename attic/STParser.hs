import Text.Peggy.PrimST

import Control.Applicative
import Control.Monad.ST
import Data.Char
import Data.HashTable.ST.Basic as HT

data ExprMemoTable s
  = ExprMemoTable
    { tbl_expr :: HT.HashTable s Int (Result Double)
    , tbl_term :: HT.HashTable s Int (Result Double)
    , tbl_fact :: HT.HashTable s Int (Result Double)
    }

instance MemoTable ExprMemoTable where
  newTable = do
    v1 <- HT.new
    v2 <- HT.new
    v3 <- HT.new
    return $ ExprMemoTable v1 v2 v3

expr :: Parser ExprMemoTable s Double
expr = memo tbl_expr $ (+) <$> term <* string "+" <*> expr <|> term

term :: Parser ExprMemoTable s Double
term = memo tbl_term $ (*) <$> fact <* string "*" <*> term <|> fact

fact :: Parser ExprMemoTable s Double
fact = memo tbl_fact $ read <$> some (satisfy isDigit)

-----

main :: IO ()
main = do
  con <- getContents
  print $ runST $ parse expr con
