module Text.Peggy.LeftRec (
  removeLeftRecursion,
  ) where

import Text.Peggy.Syntax

-- Remove only direct left recursion
-- TODO: indirect left recursion
removeLeftRecursion :: Syntax -> Syntax
removeLeftRecursion = concatMap remove where
  remove (Definition nont typ (Choice es)) | not $ null alphas =
    [ Definition nont typ $ Choice
      [ Semantic (Sequence $ beta : [NonTerminal rest]) betaFrag
      | beta <- betas
      ]
    , Definition rest ("(" ++ typ ++ ") -> (" ++ typ ++")") $ Choice $
      [ Sequence $ fs ++ [NonTerminal rest]
      | Sequence (_: fs) <- alphas
      ] ++
      [ Semantic
        (Sequence $ fs ++ [NonTerminal rest])
        (alphaFrag cf $ length (filter hasSemantic fs) + 1)
      | Semantic (Sequence (_: fs)) cf <- alphas
      ] ++
      [ Semantic Empty idFrag ]
    ]
    where
      rest = nont ++ "_tail"
      (alphas, betas) = span isLeftRec es
      
      idFrag =
        [ Snippet "id"
        ]
        
      betaFrag =
        [ Argument 2
        , Snippet " "
        , Argument 1
        ]
      
      alphaFrag org ano =
        [ Snippet "\\v999 -> "
        , Argument ano
        , Snippet " ( "
        ] ++
        map trans org ++
        [ Snippet " )" ]
      
      trans (Argument n)
        | n == 1 = Argument 999
        | otherwise = Argument (n - 1)
      trans e = e

      isLeftRec (Sequence (NonTerminal nt : _))
        = nt == nont
      isLeftRec (Semantic e _)
        = isLeftRec e
      isLeftRec _
        = False
      
      hasSemantic (Terminals _ _ _) = False
      hasSemantic (And           _) = False
      hasSemantic (Not           _) = False
      hasSemantic _                 = True
  
  remove d@(Definition nont _ (NonTerminal nt))
    | nont == nt = error "cannot remove left recursion"
    | otherwise = [d]

  remove e = [e]
