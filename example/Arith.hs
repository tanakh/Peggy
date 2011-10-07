{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

data Parser s a = Parser

parse :: (StringLike s) => Parser s a -> s -> a

[peggy|

php = stmtEmbed' : stmt*

stmtEmbed'
  = (!"<?php" .)* <* ("<?php" / eof)

stmt
  = "for" "(" exp? ";" exp? ";" exp? ")" stmt { StmtFor $1 $2 $3 $4 }
  / exp sep { StmtExp $1 }
  / stmtEmbed

sep = (";" / &"<?php")

|]

[peggy|
progn = stmt*

stmt
  = "for" "(" exp? ";" exp? ";" exp? ")" stmt
  / "while" "(" exp ")" stmt
  / "{" stmt "}"

expr
  = expr ("*" / "/") expr
  / expr ("+" / "-") expr

term
  = [0-9]+ { read }
  / 
|]

main :: IO ()
main = undefined
