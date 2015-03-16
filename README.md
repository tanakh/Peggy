# Peggy: A Parser Generator of Parsing Expression Grammer (PEG) #

# About

This is an yet another parser generator for Parsing Expression Grammer (PEG) which is:

* Simple
* Concise
* Fast
* Modern

# Usage

Recent stable releases for Peggy are available from Hackage:

http://hackage.haskell.org/package/peggy

Follow these instructions to install:

~~~ {.bash}
$ cabal update
$ cabal install Peggy
~~~

# Why should you use Peggy?

Haskell has several commonly-used parser generators, for example Alex/Happy.

Peggy attempts to improve on these shortcomings of Alex/Happy:

* Generates legacy codes.
* Does not employ modern Haskell libraries and classes such as monad-transformers, iteratee, ListLike, Text, et cetera.
* Traditional Regexp/CFG based parser.
* Parsec has no good error recovery.

unnun, kannun...

...

# Quick Start

Here is an example of parsing arithmetic expressions.

~~~ {.haskell}
{-# QuasiQuotes #-}
{-# Language FlexibleContexts #-}

import Text.Peggy

[peggy|
exp :: Double
  = exp "+" fact  { $1 + $2 }
  / exp "-" fact  { $1 - $2 }
  / fact
fact :: Double
  = fact "*" term { $1 * $2 }
  / fact "/" term { $1 / $2 }
  / term
term :: Double
  = "(" exp ")"
  / number
number ::: Double
  = ([1-9][0-9]*) { read $1 }
|]

main :: IO ()
main =
  print . parse exp =<< getContents
~~~
