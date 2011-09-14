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
    udvChar = (mkName "udv_char", NotStrict, ) <$> [t| Result $(conT derivsName) Char |]
    udvPos  = (mkName "udv_pos" , NotStrict, ) <$> [t| SrcPos |]
  toDV (Definition nont typ _) = do
    t <- [t| Result $(conT derivsName) $(parseType' typ) |]
    return (mkName $ "udv_" ++ nont, NotStrict, t)

  instDerivs =
    instanceD (cxt []) (conT ''Derivs `appT` conT derivsName)
    [ valD (varP 'dvPos) (normalB $ varE $ mkName "udv_pos") []
    , valD (varP 'dvChar) (normalB $ varE $ mkName "udv_char") []
    , funD 'parse [clause [varP pos, varP str] (normalB [| $(varE d) |]) implParse]
    ]
    where
    implParse =
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

    c = mkName "c"
    cs =  mkName "cs"
    d = mkName "d"
    pchar = mkName "pchar"
    pos = mkName "pos"
    str = mkName "str"

    pp = map (\(Definition nont _ _) -> [| unParser $(varE $ mkName $ nont) $(varE d) |])
  
  parsers = concatMap gen defs
  
  gen (Definition nont typ e) =
    [ sigD (mkName nont) [t| Parser $(conT derivsName) $(parseType' typ) |]
    , funD (mkName nont)
      [clause [] (normalB $ genP e) []]]
  
  genP e = case e of
    Terminals False False str ->
      [| string str |]
    Terminals True  False str ->
      [| many $(varE skip) *> string str |]
    Terminals False True  str ->
      [| string str <* many $(varE skip) |]
    Terminals True True  str ->
      [| many $(varE skip) *> string str <* many $(varE skip) |]

    TerminalSet rs ->
      [| satisfy $(genRanges rs) |]
    TerminalCmp rs ->
      [| satisfy $ not . $(genRanges rs) |]
    TerminalAny ->
      [| anyChar |]
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

    -- these are removed normalized phase
    SepBy  {} -> error "internal desugar error"
    SepBy1 {} -> error "internal desugar error"

    Named {} -> error "named expr must has semantic."

    where
      skip = mkName "skip"

      genBinds _ [] = []
      genBinds ix (f:fs) = case f of
        Named name g ->
          bindS (asP (mkName name) $ varP $ mkName (var ix)) (genP g) :
          genBinds (ix+1) fs
        _ | shouldBind f ->
          bindS (varP $ mkName $ var ix) (genP f) :
          genBinds (ix+1) fs
        _ ->
          noBindS (genP f) :
          genBinds ix fs

      shouldBind f = case f of
        Terminals _ _ _ -> False
        And _ -> False
        Not _ -> False
        _ -> True

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

  parseType' typ =
    case parseType typ of
      Left err -> error $ "type parse error :" ++ typ ++ ", " ++ err
      Right t -> case t of
        -- GHC.Unit.() is not a type name. Is it a bug of haskell-src-meta?
        -- Use (TupleT 0) insted.
        ConT con | show con == "GHC.Unit.()" ->
          return $ TupleT 0
        _ ->
          return t

  var n = "v" ++ show (n :: Int)
