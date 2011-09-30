{-# Language TemplateHaskell #-}
{-# Language TupleSections #-}
{-# Language FlexibleContexts #-}

module Text.Peggy.CodeGen.TH (
  genDecs,
  genQDecs,
  genQQ,
  ) where

import Control.Applicative
import Control.Monad
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.ListLike as LL
import Data.Typeable
import qualified Language.Haskell.Interpreter as HINT
import Language.Haskell.Meta
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Peggy.Prim
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc
import Text.Peggy.Normalize
import Text.Peggy.LeftRec

qqsuf :: String
qqsuf = "_QQ"

genQQ :: Syntax -> (String, String) -> Q [Dec]
genQQ syn (qqName, parserName) = do
  sig <- sigD (mkName qqName) (conT ''QuasiQuoter)
  dat <- valD (varP $ mkName qqName) (normalB con) []
  return [sig, dat]
  where
    con = do
      e <- [| \str -> do
               loc <- location
               case parse $(varE $ mkName $ parserName ++ qqsuf) (SrcPos (loc_filename loc) 0 (fst $ loc_start loc) (snd $ loc_start loc)) str of
                 Left err -> error $ show err
                 Right a -> a
            |]
      u <- [| undefined |]
      recConE 'QuasiQuoter [ return ('quoteExp, e)
                           , return ('quoteDec, u)
                           , return ('quotePat, u)
                           , return ('quoteType, u)
                           ]

genDecs :: Syntax -> Q [Dec]
genDecs = generate . removeLeftRecursion . normalize

genQDecs :: Syntax -> Q [Dec]
genQDecs = generateQ . removeLeftRecursion . normalize

-- TODO: can merge generate and generateQ?
generate :: Syntax -> Q [Dec]
generate defs = do
  tblTypName <- newName "MemoTable"
  tblDatName <- newName "MemoTable"
  ps <- parsers tblTypName
  sequence $ [ defTbl tblTypName tblDatName
             , instTbl tblTypName tblDatName
             ] ++ ps
  where
  n = length defs
  
  defTbl tblTypName tblDatName = do
    s <- newName "s"
    str <- newName "str"
    dataD (cxt []) tblTypName [PlainTV str, PlainTV s] [con s str] []
    where
      con s str = recC tblDatName $ map toMem defs where
        toMem (Definition nont typ _) = do
          t <- [t| HT.HashTable $(varT s) Int
                   (Result $(varT str) $(parseType' typ)) |]
          return (mkName $ "tbl_" ++nont, NotStrict, t)

  instTbl tblTypName tblDatName = do
    str <- newName "str"
    instanceD (cxt []) (conT ''MemoTable `appT` (conT tblTypName `appT` varT str))
      [ valD (varP 'newTable) (normalB body) [] ]
    where
    body = do
      names <- replicateM n (newName "t")
      doE $ map (\name -> bindS (varP name) [| HT.new |]) names
            ++ [ noBindS $ appsE [varE 'return, appsE $ conE tblDatName : map varE names]]

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
    Terminals True True  str ->
      genP (Token (Terminals False False str))

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

    Semantic (Sequence es) cf -> do
      let needSt = hasPos cf || hasSpan cf
          needEd = hasSpan cf
          st = if needSt then [bindS (varP $ mkName stName) [| getPos |]] else []
          ed = if needEd then [bindS (varP $ mkName edName) [| getPos |]] else []
      doE $ st ++ genBinds 1 es ++ ed ++ [ noBindS [| return $(genCF cf) |] ]
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
      [| token $(varE skip) $(varE delimiter) ( $(genP f) ) |]

    -- these are removed normalized phase
    SepBy  {} -> error "internal desugar error"
    SepBy1 {} -> error "internal desugar error"

    -- simply, ignoreing result value
    Named "_" f ->
      [| () <$ $(genP f) |]

    Named {} -> error "named expr must has semantic."

    where
      skip = mkName "skip"
      delimiter = mkName "delimiter"

      genBinds _ [] = []
      genBinds ix (f:fs) = case f of
        Named "_" g ->
          noBindS (genP g) :
          genBinds ix fs
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
    toStr (Argument a)  = var a
    toStr (AntiArgument _) = undefined
    toStr ArgPos = "(LocPos " ++ stName ++ ")"
    toStr ArgSpan = "(LocSpan " ++ stName ++ " " ++ edName ++ ")"

  hasPos  = any (==ArgPos)
  hasSpan = any (==ArgSpan)

  var n = "v" ++ show (n :: Int)
  stName = "st_Pos"
  edName = "ed_Pos"

generateQ :: Syntax -> Q [Dec]
generateQ defs = do
  tblTypName <- newName "MemoTable_QQ"
  tblDatName <- newName "MemoTable_QQ"
  ps <- parsers tblTypName
  sequence $ [ defTbl tblTypName tblDatName
             , instTbl tblTypName tblDatName
             ] ++ ps
  where
  n = length defs
  
  defTbl tblTypName tblDatName = do
    s <- newName "s"
    str <- newName "str"
    dataD (cxt []) tblTypName [PlainTV str, PlainTV s] [con s str] []
    where
      con s str = recC tblDatName $ map toMem defs where
        toMem (Definition nont typ _) = do
          t <- [t| HT.HashTable $(varT s) Int
                   (Result $(varT str) ExpQ) |]
          return (mkName $ "tbl_" ++ nont ++ qqsuf, NotStrict, t)

  instTbl tblTypName tblDatName = do
    str <- newName "str"
    instanceD (cxt []) (conT ''MemoTable `appT` (conT tblTypName `appT` varT str))
      [ valD (varP 'newTable) (normalB body) [] ]
    where
    body = do
      names <- replicateM n (newName "t")
      doE $ map (\name -> bindS (varP name) [| HT.new |]) names
            ++ [ noBindS $ appsE [varE 'return, appsE $ conE tblDatName : map varE names]]

  parsers tblName = concat <$> mapM (gen tblName) defs
  
  gen tblName (Definition nont typ e) = do
    str <- newName "str"
    s <- newName "s"
    return $
      [ sigD (mkName $ nont ++ qqsuf) $
        forallT [PlainTV str, PlainTV s]
                (cxt [classP ''LL.ListLike [varT str, conT ''Char]]) $
        conT ''Parser `appT`
        (conT tblName `appT` varT str) `appT`
        varT str `appT`
        varT s `appT`
        [t| ExpQ |]
      , funD (mkName $ nont ++ qqsuf)
        [clause [] (normalB [| memo $(varE $ mkName $ "tbl_" ++ nont ++ qqsuf) $ $(genP e) |]) []]]
  
  genP e = case e of
    Terminals False False str ->
      [| lift <$> string str |]
    Terminals True True  str ->
      genP (Token (Terminals False False str))

    TerminalSet rs ->
      [| lift <$> satisfy $(genRanges rs) |]
    TerminalCmp rs ->
      [| lift <$> (satisfy $ not . $(genRanges rs)) |]
    TerminalAny ->
      [| lift <$> anyChar |]
    NonTerminal nont ->
      [| $(varE $ mkName $ nont ++ qqsuf) |]
    Primitive name ->
      [| lift <$> $(varE $ mkName name) |]
    Empty ->
      [| lift <$> return () |]

    Semantic (Sequence es) cf -> do
      bs <- sequence $ genBinds 1 es
      let vn = length $ filter isBind bs
      let gcf = genCF $ [Snippet $ "\\" ++ unwords (names vn ++ qames vn) ++ " -> ("] ++ cf ++ [Snippet ")"]
      doE $ map return bs ++
            [ noBindS [| return $ foldl appE (return $(lift =<< gcf)) $(eQnames vn) |]]
      where
        eQnames nn =
          listE $ [ [| $(varE $ mkName $ var i) |] | i <- [1..nn]] ++
                  [ if hasAQ i cf
                    then [| parseExp' =<< eval . pprint =<<  $(varE $ mkName $ var i) |]
                    else [| lift (0 :: Int) |]
                  | i <- [1..nn]]
        names nn = map var [1..nn]
        qames nn = map qar [1..nn]

    Semantic f cf ->
      genP (Semantic (Sequence [f]) cf)

    Sequence es -> do
      binds <- sequence $ genBinds 1 es
      let vn = length $ filter isBind binds
      doE $ do
        map return binds ++
          [ noBindS [| return $(foldl fmapE (lamN vn) $ map varE (names vn)) |] ]
      where
        names nn = map (mkName . var) [1..nn]
        lamN nn = lamE (map varP $ names n) $ tupE (map varE $ names nn)
        fmapE a b = [| $a `appE` $b |]

    Choice es ->
      [| $(foldl1 (\a b -> [| $a <|> $b |]) $ map genP es) |]

    Many f ->
      [| do eQs <- many $(genP f); return $ listE eQs |]
    Some f ->
      [| do eQs <- some $(genP f); return $ listE eQs |]
    Optional f ->
      [| do qm <- optional $(genP f); case qm of Nothing -> lift Nothing; Just q -> do ee <- q; lift (Just ee) |]
    And f ->
      [| lift () <$ expect $(genP f) |]
    Not f ->
      [| lift () <$ unexpect $(genP f) |]

    Token f ->
      [| token $(varE skip) $(varE delimiter) ( $(genP f) ) |]

    -- these are removed normalized phase
    SepBy  {} -> error "internal desugar error"
    SepBy1 {} -> error "internal desugar error"

    -- simply, ignoreing result value
    Named "_" f ->
      [| lift () <$ $(genP f) |]

    Named {} -> error "named expr must has semantic."

    where
      skip = mkName "skip"
      delimiter = mkName "delimiter"

      genBinds _ [] = []
      genBinds ix (f:fs) = case f of
        Named "_" g ->
          noBindS (genP g) :
          genBinds ix fs
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

  genCF cf = parseExp' scf where
    scf = concatMap toStr cf
    toStr (Snippet str) = str
    toStr (Argument a)  = var a
    toStr (AntiArgument a) = qar a
    toStr ArgPos = "(LocPos " ++ stName ++ ")"
    toStr ArgSpan = "(LocSpan " ++ stName ++ " " ++ edName ++ ")"

  hasAQ x cf = not . null $ filter (isAQ x) cf where
    isAQ i (AntiArgument j) = i == j
    isAQ _ _ = False

  hasPos  = any (==ArgPos)
  hasSpan = any (==ArgSpan)

  var n = "v" ++ show (n :: Int)
  qar n = "q" ++ show (n :: Int)
  stName = "st_Pos"
  edName = "ed_Pos"

parseExp' str =
  case parseExp str of
    Left _ ->
      error $ "code fragment parse error: " ++ str
    Right ret ->
      return ret

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

eval :: Typeable a => String -> Q a
eval str = do
  res <- runIO $ HINT.runInterpreter $ do
    HINT.setImports ["Prelude"]
    HINT.interpret str HINT.infer
  case res of
    Left err -> error $ show err
    Right ret -> return ret
