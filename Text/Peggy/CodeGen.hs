{-# Language TemplateHaskell #-}

module Text.Peggy.CodeGen (
  genCode,
  removeLeftRecursion,
  ) where

import Language.Haskell.Meta
import Language.Haskell.TH
import Text.Peggy
import Text.Peggy.Syntax
import Text.Peggy.SrcLoc
import Text.Peggy.Normalize
import Text.Peggy.LeftRec

genCode :: Syntax -> [Dec]
genCode = generate . removeLeftRecursion . normalize

generate :: Syntax -> [Dec]
generate defs = defDerivs : instDerivs : parse : parsers where
  derivsName = mkName "UserDerivs"
  
  defDerivs = DataD [] derivsName [] [con] [] where
    con = RecC derivsName $ udvChar : udvPos : map toDV defs
    
    udvChar = (mkName "udv_char", NotStrict,
               AppT (AppT (ConT ''Result) (ConT derivsName)) (ConT ''Char))
    udvPos  = (mkName "udv_pos",  NotStrict, ConT ''SrcLoc)
    
    toDV (Definition nont typ _) =
      ( mkName ("udv_" ++ nont)
      , NotStrict
      , derivType typ
      )
  
  instDerivs =
    InstanceD [] (AppT (ConT ''Derivs) $ ConT derivsName)
    [ ValD (VarP 'dvPos) (NormalB $ VarE $ mkName "udv_pos") []
    , ValD (VarP 'dvChar) (NormalB $ VarE $ mkName "udv_char") []
    ]
  
  parse = 
    FunD fname
    [Clause [VarP pos, VarP str]
     (NormalB $ VarE d)
     [ ValD (VarP d)
       (NormalB $ foldl1 AppE $ [ConE derivsName, VarE pchar, VarE pos] ++ pp)
       []
     , ValD (VarP pchar)
       (NormalB $ CaseE (VarE str)
        [ Match
          (InfixP (VarP c) '(:) (VarP cs))
          (NormalB $ app2c 'Parsed (app2 fname (app2 'advance (VarE pos) (VarE c)) (VarE cs)) (VarE c))
          []
        , Match
          WildP
          (NormalB $ app1c 'Failed (app2c 'ParseError (app1 'dvPos (VarE d)) (LitE (StringL ""))))
          []
        ])
       []
      ]
    ]
    where
      fname = mkName "parse"
      c = mkName "c"
      cs =  mkName "cs"
      d = mkName "d"
      pchar = mkName "pchar"
      pos = mkName "pos"
      str = mkName "str"

      pp = map
        (\(Definition nont _ _) ->
          app2 'unParser (VarE $ mkName nont) (VarE d))
        defs
  
  parsers = concatMap gen defs
  
  gen (Definition nont typ e) =
    [ SigD (mkName nont) $ parserType typ
    , FunD (mkName nont)
     [Clause [] (NormalB $ genP e) []]]
  
  genP e = case e of
    Terminals str ->
      app1 'string $ strLit str
    TerminalSet rs ->
      app1 'satisfy $ genRanges rs
    NonTerminal nont ->
      app1c 'Parser $ VarE (mkName $ "udv_" ++ nont)
    Empty ->
      app1 'return $ TupE []
    
    Semantic (Sequence es) cf ->
      DoE $ genBinds 1 es ++ [ NoBindS $ app1 'return $ genCF cf ]
    Semantic f cf ->
      DoE [ BindS (VarP $ mkName $ var 1) (genP f)
          , NoBindS $ app1 'return $ genCF cf ]

    Sequence es ->
      let binds = genBinds 1 es in
      let vn = length $ filter isBind binds in
      DoE $ binds ++
            [ NoBindS $ app1 'return
                      $ TupE $ map (VarE . mkName . var) [1..vn] ]

    Choice es ->
      foldl1 (app2 '(<|>)) $ map genP es

    Many f ->
      app1 'many $ genP f

    Some f ->
      app1 'some $ genP f

    Optional f ->
      app1 'optional $ genP f

    And f ->
      app1 'expect $ genP f

    Not f ->
      app1 'unexpect $ genP f

    where
      genBinds _ [] = []
      genBinds ix (f:fs) =
        case f of
          Terminals _ ->
            NoBindS (genP f) :
            genBinds ix fs
          _ ->
            BindS (VarP $ mkName $ var ix) (genP f):
            genBinds (ix+1) fs

      isBind (BindS _ _) = True
      isBind _ = False
  
  genRanges rs =
    let c = mkName "c" in
    LamE [VarP c] $ foldl1 (app2 '(||)) $ map (genRange $ VarE c) rs
  genRange c (CharRange l h) =
    app2 '(&&) (app2 '(<=) (charLit l) c)
               (app2 '(<=) c (charLit h))
  genRange c (CharOne v) =
    app2 '(==) c (charLit v)

  genCF cf =
    case parsed of
      Left _ ->
        error $ "code fragment parse error: " ++ scf
      Right ret ->
        ret
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
  
  app1 f x   = AppE (VarE f) x
  app2 f x y = AppE (AppE (VarE f) x) y
  app1c f x   = AppE (ConE f) x
  app2c f x y = AppE (AppE (ConE f) x) y

  strLit = LitE . StringL
  charLit = LitE . CharL
