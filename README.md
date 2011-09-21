# Peggy: A Parser Generator of Parsing Expression Grammer (PEG) #

# About

This is an yet another parser generator of Parsing Expression Grammer (PEG) which is:

* Simple
* Concise
* Fast
* Modern

# Usage

You can find a recent stable release in Hackage DB.
You can install this as following instruction:

    $ cabal update
    $ cabal install Peggy

# Why should you use Peggy?

Haskell has commonly used parser generators, one of them are Alex/Happy.
But I think Alex/Happy are not good in these points:

* Generates regacy codes

Alex uses only too basic libraries.
It does not use monad-transformers, iteratee, ListLike, Text, and so on.

* Tradisional Regexp/CFG based parser

Parsec has no good error recovery.

unnun, kannun...

...

# Quick Start

Here is an example of parsing arithmetic expressions.

    {-# QuasiQuotes #-}
    import Text.Peggy
    
    [peggy|
    exp  = exp "+" fact    { $1 + $2 }
         / exp "-" fact    { $1 - $2 }
         / fact
    fact = fact "*" term   { $1 * $2 }
         / fact "/" term   { $1 / $2 }
         / term
    term = "(" exp ")"
         / number
    number = ([1-9][0-9]*) { read $1 }
    |]
    
    main :: IO ()
    main =
      print =<< parse exp =<< getContents

...
