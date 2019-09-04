{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Match where

import           Control.Monad             (when)
import           Data.Expr.Types           (Pattern (..))
import           Data.List.NonEmpty        (NonEmpty (..))
import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

ruleMatch :: Rule -> Ctx -> Text -> Typed -> (Text, Text, Text) -> Typed -> Gen ProofTree
ruleMatch dispatch q x e1 (x1, x2, x3) e2 =
  do
    setRuleName "match"

    let u = def

    ((_, τx), p) <- splitCtx' u q x
    (_, r) <- splitCtx' u q x
    r' <- augmentCtx u r [(x1, tyTree), (x2, tyNat), (x3, tyTree)]

    -- r_{(\vec{a}, a, a, b)} = q_{(\vec{a}, a, b)}
    {-
    forVec_ q (only1 x `after` dropAllBut [x] q) $ \((a1, c1), qab) ->
      forVec_ r' (only2 x1 x3 `after` dropAllBut [x1, x3] q) $ \((a2, a3, c2), raab) ->
        when (and [a1 == a2, a2 == a3, c1 == c2]) $
          accumConstr [ CEq (CAtom qab) (CAtom raab) ]
    -}

    -- p_{(\vec{a}, c)} = \sum_{a+b=c} q_{(\vec{a}, a, b)}
    {-
    forVec_ q (only1 x `after` dropAllBut [x] q) $ \((a, b), qab) ->
      forVec_ p (onlyCost `after` dropCtxVars q) $ \(c, pc) ->
        when (c == a + b) $
          accumConstr [ CEq (CAtom qab) (CAtom pc) ]
    -}

    -- r_{m+1} = r_{m+2} = q_{m+1}
    let m = Ctx.length q - 1
    rx1 <- coeff r' (RankIdx $ m + 1)
    rx3 <- coeff r' (RankIdx $ m + 2)
    qx <- coeff q (RankIdx $ m + 1)
    accumConstr
      [ CEq (CAtom rx1) (CAtom rx3)
      , CEq (CAtom rx3) (CAtom qx)
      ]

    -- r_{(\vec{0}, 1, 0, 0)} = r_{(\vec{0}, 0, 1, 0)} = q_{m+1}
    {-
    forVec_ r' (only2 x1 x3 `after` dropCtxVars q) $ \(a, r100) ->
      forVec_ r' (only2 x1 x3 `after` dropCtxVars q) $ \(b, r010) ->
        case (a, b) of
          ((1, 0, 0), (0, 1, 0)) ->
            accumConstr [ CEq (CAtom r100) (CAtom r010)
                        , CEq (CAtom r010) (CAtom qx)
                        ]
          _ -> return ()
    -}

    -- recursive calls
    q1' <- prove dispatch p  e1
    q2' <- prove dispatch r' e2

    -- equate "return" contexts
    q' <- returnCtx u

    eqCtx q1' q2'
    eqCtx q2' q'

    -- TODO: fix expression
    conclude q (TyMatch (TyVar x, τx) ((PNil, (hole, tyHole)) :| [(PNode x1 x2 x3, (hole, tyHole))])) q'
