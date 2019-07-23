{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Match where

import           Data.Expr.Types           (Pattern (..))
import           Data.List.NonEmpty        (NonEmpty (..))
import           Lac.Analysis.Rules.Common

ruleMatch :: Rule -> Ctx -> Text -> Typed -> (Text, Text, Text) -> Typed -> Gen ProofTree
ruleMatch dispatch q x e1 (x1, x2, x3) e2 =
  do
    setRuleName "match"

    let u = Bound 1
    let m = lengthCtx q - 1

    ((_, τx), p) <- splitCtx' u q x
    (_, r) <- splitCtx' u q x
    r' <- augmentCtx u r [(x1, tyTree), (x2, tyNat), (x3, tyTree)]

    -- TODO: r(vec{a}, a, a, b) = q(vec{a}, a, b)

    -- TODO: p(vec{a}, c) = sum{a+b=c} q(vec{a}, a, b)

    -- TODO: r_{m+1} = r_{m+2} = q_{m+1}

    -- TODO: r_{(\vec{0}, 1, 0, 0)} = r_{(\vec{0}, 0, 1, 0)} = q_{m+1}

    -- recursive calls
    q1' <- prove dispatch p  e1
    q2' <- prove dispatch r' e2

    -- equate "return" contexts
    q' <- returnCtx u

    eqCtx q1' q2'
    eqCtx q2' q'

    -- TODO: fix expression
    conclude q (TyMatch (TyVar x, τx) ((PNil, (hole, tyHole)) :| [(PNode x1 x2 x3, (hole, tyHole))])) q'
