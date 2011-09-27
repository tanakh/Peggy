---
title: Example
---

<div class="span14">

## Simple Arithmetic Expression Parser

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

## JSON Parser

    data JSON
      = JSONString String
      | JSONNumber Double
      | JSONObject [(String, JSON)]
      | JSONArray [JSON]
      | JSONBool Bool
      | JSONNull
      deriving (Show)
    
    [peggy|
    jsons :: [JSON]
      = json* !.
    
    json :: JSON
      = object / array
    
    object :: JSON
      = "{" (pair, ",") "}" { JSONObject $1 }
    
    pair :: (String, JSON)
      = jsstring ":" value
    
    array :: JSON
      = "[" (value, ",") "]" { JSONArray $1 }
    
    value :: JSON
      = jsstring { JSONString $1 }
      / number   { JSONNumber $1 }
      / object
      / array
      / "true"   { JSONBool True }
      / "false"  { JSONBool False }
      / "null"   { JSONNull }
    
    jsstring ::: String
      = '\"' jschar* '\"'
    
    jschar :: Char
      = '\\' escChar / [^\"\\\]
    
    escChar :: Char
      = '\"' { '\"' }
      / '\\' { '\\' }
      / '/' { '/' }
      / 'b' { '\b' }
      / 'f' { '\f' }
      / 'n' { '\n' }
      / 'r' { '\r' }
      / 't' { '\t' }
      / 'u' hex hex hex hex { chr $ fst $ head $ readHex [$1, $2, $3, $4] }
    
    hex :: Char = [0-9a-zA-Z]
    
    number ::: Double
      = [1-9] [0-9]* { read ($1 : $2) }
      / [0]          { 0.0 }
    |]

</div>
