module Text.Peggy.Normalize (
  normalize,
  shouldBind,
  ) where

import Data.List
import Text.Peggy.Syntax

normalize :: Syntax -> Syntax
normalize = map desugarDef . addSkipDelim

addSkipDelim :: Syntax -> Syntax
addSkipDelim defs = skp ++ dlm ++ defs
  where
    skp | hasSkip = []
        | otherwise = [defaultSkipImpl]
    dlm | hasDelim = []
        | otherwise = [defaultDelimImpl]

    hasSkip = not $ null [ () | Definition nont _ _ <- defs , nont == "skip" ]
    hasDelim = not $ null [ () | Definition nont _ _ <- defs , nont == "delimiter" ]
    
    defaultSkipImpl =
      Definition "skip" "()" $ Primitive "space"
    defaultDelimImpl =
      Definition "delimiter" "()" $ Primitive "defaultDelimiter"

desugarDef :: Definition -> Definition
desugarDef (Definition nont typ expr) =
  Definition nont typ (desugar expr)
  where
    desugar e = case e of
      Terminals True True str ->
        Token $ Terminals False False str
        
      Terminals {} -> e
      TerminalSet {} -> e
      TerminalCmp {} -> e
      TerminalAny {} -> e
      NonTerminal {} -> e
      Primitive {} -> e
      Empty -> e
      
      Named name f -> Named name $ desugar f
      
      Choice es -> Choice $ map desugar es
      Many f -> Many $ desugar f
      Some f -> Some $ desugar f
      Optional f -> Optional $ desugar f
      And f -> And $ desugar f
      Not f -> Not $ desugar f
      
      Sequence es ->
        desugar $ Semantic (Sequence es) $ defaultCF $ length $ filter shouldBind es
      
      Semantic (Sequence es) cf ->
        Semantic (Sequence $ map desugar es) cf
      Semantic f cf ->
        Semantic (Sequence [desugar f]) cf
      
      SepBy f g ->
        desugar (Choice [SepBy1 f g, Semantic Empty [Snippet "[]"]])
        
      SepBy1 f g ->
        let f' = desugar f in
        let g' = desugar g in
        let g'' = desugar $ Semantic g' [Snippet "()"] in
        Semantic (Sequence [f', (Many (Semantic (Sequence [g'', f']) [Argument 2]))])
        [ Argument 1
        , Snippet ":"
        , Argument 2
        ]
      
      Token f ->
        Token $ desugar f

    defaultCF n =
      [ Snippet "(" ] ++
      intersperse (Snippet ",") (map Argument[1..n]) ++
      [ Snippet ")" ]

shouldBind f = case f of
  Terminals _ _ _ -> False
  And _ -> False
  Not _ -> False
  Token g -> shouldBind g
  _ -> True
