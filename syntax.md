---
title: Syntax
---

<div class="span14">

## Syntax of Peggy

Peggy support [PEG][PAC] and a notation based on [this wikipedia page][PEG].

### Self definition of Peggy syntax

[Here][BOOT] is complete definition of Peggy grammer which is used for bootstrapping.

## Lexical Rule

Identifier of Peggy syntax is alphabet and digit sequence starting lower-case letter.

    ident ::: String = [a-z] [0-9a-zA-Z_]* { $1 : $2 }

Peggy syntax is ignore spaces. Space is just a delimiter. Any code layout can be allowd.

### Comment

Haskell-like comment are allowed.

    -- This is comment
    hoge = ...
    {- region comment {- and nested comment -} are also usable -}
    moge = ...

## Definition

Peggy syntax is some definitions of nonterminal symbol.
Each definition is looks like this:

    <name> :: <Type> = <expr>

or another syntax of definition:

    <name> ::: <Type> = <expr>

<name> is name of nonterminal. <Type> must be specified. Type is separated by double-colon (::) or triple-colon (:::). Triple-colon has special meaning, this nonterminal is a _token_ which is a self delimited term. Specification of _token_ is described [here](#token). It is Haskell's type and is a result type of this nonterminal symbol. Definition of nonterminal is some alternatives. These are separated by '/'. Each alternative is sequence of grammers.

Here is typical form of a nondeterminal definition.

    <name> :: <Type>
      = sequence ...
      / sequence ...
      ...

### Left recursion

Peggy allows [left recursion][LREC] by transforming a grammer to a grammer which does not contain left recursions.

## Expression

Syntax of expression is based on PEG's one. An expression is constructed by following rules.

### String Literal

String literal matches exact same sequence of chars.

    'hoge' -- char sequence
    "hoge" -- string token

A string quoted by single-quote (') is raw-literal, which matches exact same char sequence.
A string quoted by double-quote (") is string _token_. It means delimited string. A particular behaviour of token is descrived [here](#token).

String literal has no value.

### Charset

An usual character notation like [regular expression][REG].

    [a-zA-Z0-9_] -- lower letters, upper letters, digits and under-score (_)
    [^0-9] -- a char except digits

Charset has a character value.

### Ordered Choice

_Ordered choice_ is sequence of expression separated by '/'.

    <e1> / <e2> /  ...

It accepts any alternative. In contrast to [CFG][CFG], PEG select a most left match always. So the order of choice is meaningfully. The type of all sub-expression must be same.

### Sequence

_Sequence_ is a sequence of expression.

    <e1> <e2> ...

### Semantic Annotation

Each _sequence_ can has semantic rule. It specify a return value of the term.

    <e1> <e2> ... { Haskell Expression ... }

Haskell expression specify the return value of expression. It must be a single Haskell expression. Here is an example:

    expr :: Int
      = expr "+" term { $1 + $2 }

Haskell expression can contain _placeholder_ which forms $\<num\>. The number means the n-th value of sub-expression. _Literal_ and _predicate_ do not has a value, so it is skipped. In above example, $1 indicates expr's value and $2 indicates term's value.

_Sequence_ without semantic annotation has a value of tupple of subterms.

    hoge :: (Int, Int, Int)
      = integer "," integer "," integer

### Zero or more, One or more, Optional (Zero or One)

The suffix '*' means zero or more repetition.

    <e>*

The suffix '+' means one or more repetition.

    <e>+

The suffix '?' means one or zero occursion.

    <e>?

Unlike in CFG, these operator always behave greedily, these match as long as possible.

'*' and '+' returns a list of the value.
'?' returns a value typed _Maybe a_.

### And predicate, Not predicate

The prefix '?' means _syntax predicates_. It look-ahead an expression and if fail to parse, the predicate fails, too. When it success, predicate successes too, but it consume no input.

    &<e>

The prefix '!' is inverse of and predicate. It fails when look-ahead success.

    !<e>

Here is an example of usage of predicate, it parses C-like nested comment.

    regionComment :: ()
      = '/*' (regionComment / (!"*/" . { () }))* '*/' { () }

## Token behaviour

Peggy recognizes (implicit) _token_. _Token_ is self delimited character sequence. It ignores pre/post white spaces. It must be delimited (token "for" does not match to character sequence "foreach").

White space and delimtier semantic can cahnge by defining two special nonterminals, which are "space" and "delimiter".

    space :: () = [ \r\n\t] { () } / lineComment / recionComment
    delimiter :: () = [()[]{}<>;:,./\] { () }

If these nonterminals are not defined, default implementation is used.

</div>

[PAC]: http://pdos.csail.mit.edu/~baford/packrat/
[PEG]: http://en.wikipedia.org/wiki/Parsing_expression_grammar
[CFG]: http://en.wikipedia.org/wiki/Context-free_grammar
[REG]: http://en.wikipedia.org/wiki/Regular_expression
[BOOT]: https://github.com/tanakh/Peggy/blob/master/bootstrap/peggy.peggy
[LREC]: http://en.wikipedia.org/wiki/Left_recursion
