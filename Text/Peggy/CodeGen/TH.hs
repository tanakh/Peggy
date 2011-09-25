{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}

module Text.Peggy.CodeGen.TH (
  genDecs,
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.ListLike as LL
import Language.Haskell.Meta
import Language.Haskell.TH
import Text.Peggy.Prim
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc
import Text.Peggy.Normalize
import Text.Peggy.LeftRec

genDecs :: Syntax -> Q [Dec]
genDecs = generate . removeLeftRecursion . normalize

generate :: Syntax -> Q [Dec]
generate defs = do
  tblName <- newName "MemoTable"
  ps <- parsers tblName
  sequence $ [defTbl tblName, instTbl tblName] ++ ps
  where
  n = length defs
  
  defTbl tblName = do
    s <- newName "s"
    str <- newName "str"
    dataD (cxt []) tblName [PlainTV str, PlainTV s] [con s str] []
    where
      con s str = recC tblName $ map toMem defs where
        toMem (Definition nont typ _) = do
          t <- [t| HT.HashTable $(varT s) Int
                   (Result $(varT str) $(parseType' typ)) |]
          return (mkName $ "tbl_" ++nont, NotStrict, t)

  instTbl tblName = do
    str <- newName "str"
    instanceD (cxt []) (conT ''MemoTable `appT` (conT tblName `appT` varT str))
      [ valD (varP 'newTable) (normalB body) [] ]
    where
    body = do
      names <- replicateM n (newName "t")
      doE $ map (\name -> bindS (varP name) [| HT.new |]) names
            ++ [ noBindS $ appsE [varE 'return, appsE $ conE tblName : map varE names]]

  parsers tblName = concat <$> mapM (gen tblName) defs
  
  gen tblName (Definition nont typ e) = do
    str <- newName "str"
    s <- newName "s"
    return $
      [ sigD (mkName nont) $
        forallT [PlainTV str, PlainTV s]
                (cxt [classP ''LL.ListLike [varT str, conT ''Char]]) $
        conT ''Parser `appT`
        (conT tblName `appT` varT str) `appT`
        varT str `appT`
        varT s `appT`
        parseType' typ
      , funD (mkName nont)
        [clause [] (normalB [| memo $(varE $ mkName $ "tbl_" ++ nont) $ $(genP e) |]) []]]
  
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
      [| $(varE $ mkName nont) |]
    Primitive name ->
      [| $(varE $ mkName name) |]
    Empty ->
      [| return () |]

    Semantic (Sequence es) cf ->
      doE $ genBinds 1 es ++ [ noBindS [| return $(genCF cf) |] ]
    Semantic f cf ->
      genP (Semantic (Sequence [f]) cf)

    Sequence es -> do
      binds <- sequence $ genBinds 1 es
      let vn = length $ filter isBind binds
      doE $ do
        map return binds ++
          [ noBindS [| return $ $(tupE $ map (varE .mkName  . var) [1..vn]) |] ]

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

    Token f ->
      [| many $(varE skip) *> $(genP f) <* many $(varE skip) |]

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

  var n = "v" ++ show (n :: Int)

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
