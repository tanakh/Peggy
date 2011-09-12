{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}

module Text.Peggy.CodeGen (
  genCode,
  removeLeftRecursion,
  ) where

import Control.Applicative
import Language.Haskell.Meta
import Language.Haskell.TH
import Text.Peggy
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc
import Text.Peggy.Normalize
import Text.Peggy.LeftRec

genCode :: Syntax -> Q [Dec]
genCode = generate . removeLeftRecursion . normalize

generate :: Syntax -> Q [Dec]
generate defs = do
  sequence $ [defDerivs, instDerivs] ++ parsers
  where
  derivsName = mkName "UserDerivs"

  defDerivs = dataD (cxt []) derivsName [] [con] [] where
    con = recC derivsName $ udvChar : udvPos : map toDV defs
    udvChar = (mkName "udv_char", NotStrict, ) <$>
              (conT ''Result `appT` conT derivsName `appT` conT ''Char)
    udvPos  = (mkName "udv_pos", NotStrict, ) <$> conT ''SrcPos

  toDV (Definition nont typ _) =
    (, NotStrict, derivType typ) <$> return (mkName ("udv_" ++ nont))

  instDerivs =
    instanceD (cxt []) (conT ''Derivs `appT` conT derivsName)
    [ valD (varP 'dvPos) (normalB $ varE $ mkName "udv_pos") []
    , valD (varP 'dvChar) (normalB $ varE $ mkName "udv_char") []
    , implParse
    ]

  implParse =
    funD 'parse [clause [varP pos, varP str] (normalB [| $(varE d) |])
     [ valD (varP d)
       (normalB $ appsE $ [conE derivsName, varE pchar, varE pos] ++ pp defs)
       []
     , valD (varP pchar)
       (normalB $ caseE (varE str)
        [ match (infixP (varP c) '(:) (varP cs))
          (normalB [| Parsed (parse ($(varE pos) `advance` $(varE c)) $(varE cs)) $(varE c) |])
          []
        , match wildP
          (normalB [| Failed $ ParseError (LocPos $ dvPos $(varE d)) "" |])
          []
        ])
       []
      ]
    ]
    where
      c = mkName "c"
      cs =  mkName "cs"
      d = mkName "d"
      pchar = mkName "pchar"
      pos = mkName "pos"
      str = mkName "str"

      pp = map (\(Definition nont _ _) -> [| unParser $(varE $ mkName $ nont) $(varE d) |])
  
  parsers = concatMap gen defs
  
  gen (Definition nont typ e) =
    [ sigD (mkName nont) $ return $ parserType typ
    , funD (mkName nont)
      [clause [] (normalB $ genP e) []]]
  
  genP e = case e of
    Terminals str ->
      [| string str |]
    TerminalSet rs ->
      [| satisfy $(genRanges rs) |]
    NonTerminal nont ->
      [| Parser $(varE $ mkName $ "udv_" ++ nont) |]
    Empty ->
      [| return () |]

    Semantic (Sequence es) cf ->
      doE $ genBinds 1 es ++ [ noBindS [| return $(genCF cf) |] ]
    Semantic f cf ->
      genP (Semantic (Sequence [f]) cf)

    Sequence es -> do
      binds <- sequence $ genBinds 1 es
      let vn = length $ filter isBind binds
      doE $ map return binds ++
               [ noBindS [| return $ $(tupE $ map (varE . mkName . var) [1..vn]) |] ]

    Choice es ->
      foldl1 (\a b -> [| $a <|> $b |]) $ map genP es

    Many f ->
      [| many $(genP f) |]
    Some f ->
      [| some $(genP f) |]
    Optional f ->
      [| optional $(genP f) |]
    And f ->
      [| expect $(genP f) |]
    Not f ->
      [| unexpect $(genP f) |]

    where
      genBinds _ [] = []
      genBinds ix (f:fs) =
        case f of
          Terminals _ ->
            noBindS (genP f) :
            genBinds ix fs
          And _ ->
            noBindS (genP f) :
            genBinds ix fs
          Not _ ->
            noBindS (genP f) :
            genBinds ix fs
          _ ->
            bindS (varP $ mkName $ var ix) (genP f):
            genBinds (ix+1) fs

      isBind (BindS _ _) = True
      isBind _ = False
  
  genRanges rs =
    let c = mkName "c" in
    lamE [varP c] $ foldl1 (\a b -> [| $a || $b |]) $ map (genRange c) rs
  genRange c (CharRange l h) =
    [| l <= $(varE c) && $(varE c) <= h |]
  genRange c (CharOne v) =
    [| $(varE c) == v |]

  genCF cf =
    case parsed of
      Left _ ->
        error $ "code fragment parse error: " ++ scf
      Right ret ->
        return ret
    where
    parsed = parseExp scf
    scf = concatMap toStr cf
    toStr (Snippet str) = str
    toStr (Argument n)  = var n

  parserType typ = 
    case parseType ("Parser UserDerivs (" ++ typ ++ ")") of
      Left err -> error $ "type parse error :" ++ typ ++ ", " ++ err
      Right t -> t  

  derivType typ = 
    case parseType ("Result UserDerivs (" ++ typ ++ ")") of
      Left err -> error $ "type parse error :" ++ typ ++ ", " ++ err
      Right t -> t

  var n = "v" ++ show (n :: Int)
