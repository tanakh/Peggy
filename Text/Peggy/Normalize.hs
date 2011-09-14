module Text.Peggy.Normalize (
  normalize,
  ) where

import Text.Peggy.Syntax

normalize :: Syntax -> Syntax
normalize = map desugarDef . addSkip

addSkip :: Syntax -> Syntax
addSkip defs
  | hasSkip = defs
  | otherwise = defaultSkipImpl : defs
  where
    hasSkip = not $ null [ () | Definition nont _ _ <- defs , nont == "skip" ]
    
    defaultSkipImpl =
      Definition "skip" "()" $
      Semantic (TerminalSet $ map CharOne " \r\n\t") [ Snippet "()" ]

desugarDef :: Definition -> Definition
desugarDef (Definition nont typ expr) =
  Definition nont typ (desugar expr)
  where
    desugar e = case e of
      Terminals {}-> e
      TerminalSet {} -> e
      TerminalCmp {} -> e
      TerminalAny {} -> e
      NonTerminal {} -> e
      Empty -> e
      
      Named name f -> Named name $ desugar f
      
      Sequence es -> Sequence $ map desugar es
      Choice es -> Choice $ map desugar es
      Many f -> Many $ desugar f
      Some f -> Some $ desugar f
      Optional f -> Optional $ desugar f
      And f -> And $ desugar f
      Not f -> Not $ desugar f
      
      Semantic f cf -> Semantic (desugar f) cf
      
      SepBy f g ->
        desugar (Choice [SepBy1 f g, Semantic Empty [Snippet "[]"]])
        
      SepBy1 f g ->
        let f' = desugar f in
        let g' = desugar g in
        let g'' = Semantic g' [Snippet "()"] in
        Semantic (Sequence [f', (Many (Semantic (Sequence [g'', f']) [Argument 2]))])
        [ Argument 1
        , Snippet ":"
        , Argument 2
        ]
