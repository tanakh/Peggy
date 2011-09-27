---
title: The Parser Generator for Haskell
---

<div class="row">

<div class="span5">

## What is it

Peggy is a parser generator for Haskell.
It supports [parsing expression grammer (PEG)][PEG]
which is simple and expressive.
It can generate efficient [packrat parser][PAC].

</div>

<div class="span4">

## Quick Start

You can install Peggy from [Hackage][HAC]:

    $ cabal update
    $ cabal install Peggy

Github repository is [here][REPO].

</div>

<div class="span5">

## Easy to use

[Syntax of Peggy](syntax.html) is very simple and easy to understand,
so you can start writing practical server immediately.

</div>

</div>

<div class="row">
<div class="span14">

## Parsing Expression Grammer

Peggy supports [parsing expresion grammer (PEG)][PEG], which is:

- No shift/reduce conflict
- Simple and Expressive
- Unlimited look-ahead
- Linear time parser

And Peggy extends PEG:

- Separated list
- Easy to handle source location
- Support left recursion

## Modern

Peggy generates modern Haskell code, which is:

- Monadic
- Use fast array and hashmap
- Supports [Template Haskell][TH] and [Quasiquotation][QQ]
- Supports fast string-like type ([ByteString][BS], [Text][TXT])
- User friendly error handling

## Embeded DSL

Peggy is an embeded DSL for Haskell.
You can embeded your parser in Haskell source code directly using [Template Haskell][TH] and [Quasiquotation][QQ] looks like below. The code is checked by Haskell's type-checker and you need no more separated grammer file.

    {-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
    
    import Text.Peggy
    
    [peggy|
    top :: Double = expr !.
    
    expr :: Double
      = expr "+" fact { $1 + $2 }
      / expr "-" fact { $1 - $2 }
      / fact
    
    fact :: Double
      = fact "*" term { $1 * $2 }
      / fact "/" term { $1 / $2 }
      / term
    
    term :: Double
      = "(" expr ")"
      / number
    
    number ::: Double
      = [1-9] [0-9]* { read ($1 : $2) }
    |]

    main :: IO ()
    main = print . parseString top "<stdin>" =<< getContents

## Learn More

You can find more document about Peggy [here](reference.html).

</div>
</div>

[PEG]: http://en.wikipedia.org/wiki/Parsing_expression_grammar
[PAC]: http://pdos.csail.mit.edu/~baford/packrat/
[HAC]: http://hackage.haskell.org/package/peggy
[REPO]: https://github.com/tanakh/peggy
[TH]: http://www.haskell.org/haskellwiki/Template_Haskell
[QQ]: http://www.haskell.org/haskellwiki/Quasiquotation
[BS]: http://hackage.haskell.org/package/bytestring
[TXT]: http://hackage.haskell.org/package/text
