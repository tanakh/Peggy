{-# Language QuasiQuotes #-}

import Text.Peggy

data JSON
  = JSONString String
  | JSONNumber Double
  | JSONObject [(String, JSON)]
  | JSONArray [JSON]
  | JSONBool Bool
  | JSONNull
  deriving (Show)

[peggy|
json :: JSON
  = object / array

value :: JSON
  = jsstring { JSONString $1 }
  / number   { JSONNumber $1 }
  / object
  / array
  / "true"   { JSONBool True }
  / "false"  { JSONBool False }
  / "null"   { JSONNull }

jsstring :: String
  = "\"' jschar* '\""

jschar :: Char
  = !'\"' [0-9a-zA-Z]

number :: Double
  = "' [1-9] [0-9]* '" { read ($1 : $2) }
  / "' [0]          '" { 0.0 }

object :: JSON
  = "{" (pair, ",") "}" { JSONObject $1 }

pair :: (String, JSON)
  = jsstring ":" value

array :: JSON
  = "[" (value, ",") "]" { JSONArray $1 }
|]

main :: IO ()
main = print . runParser json "<stdin>" =<< getContents
