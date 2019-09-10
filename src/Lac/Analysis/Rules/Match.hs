{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Match where

import           Data.Expr.Types           (Pattern (..))
import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import           Control.Monad             (when)
import qualified Data.List.Ext             as L
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Vector               as V

import           Debug.Trace

ruleMatch :: Rule -> Ctx -> Text -> Typed -> (Text, Text, Text) -> Typed -> Gen ProofTree
ruleMatch dispatch q x e1 (x1, x2, x3) e2 =
  do
    setRuleName "match"

    let u@(Bound ub) = def
    let m = Ctx.length q - 1

    case L.splitAt m . Ctx.trees $ q of
      (_, [y]) | x == y ->
        return ()
      _ ->
        assert False $ "ruleMatch: expected variable " <> x <> " to be in last position"

    ((_, τx), p) <- splitCtx' u q x
    (_, r) <- splitCtx' u q x
    r' <- augmentCtx u r [(x1, tyTree), (x2, tyNat), (x3, tyTree)]

    -- generate all \vec{a}
    let allVecA = L.enum ub m

    -- r_{(\vec{a}, a, a, b)} = q_{(\vec{a}, a, b)}
    forM_ allVecA $ \as ->
      forM_ [0..ub] $ \a ->
        forM_ [0..ub] $ \b -> do
          let vr = VecIdx . V.fromList $ as ++ [a, a] ++ [b]
          let vq = VecIdx . V.fromList $ as ++ [a]    ++ [b]
          rv <- coeff r' vr
          qv <- coeff q vq
          accumConstr [ CEq (CAtom rv) (CAtom qv) ]

    -- p_{(\vec{a}, c)} = \sum_{a+b=c} q_{(\vec{a}, a, b)}
    forM_ allVecA $ \as ->
      forM_ [0..ub] $ \c ->
        forM_ (L.split c) $ \(a, b) -> do
          let vp = VecIdx . V.fromList $ as ++ [c]
          let vq = VecIdx . V.fromList $ as ++ [a, b]
          pv <- coeff p vp
          qv <- coeff q vq
          accumConstr [ CEq (CAtom pv) (CAtom qv) ]

    -- r_{m+1} = r_{m+2} = q_{m+1}
    rx1 <- coeff r' (RankIdx $ m + 1)
    rx3 <- coeff r' (RankIdx $ m + 2)
    qx <- coeff q (RankIdx $ m + 1)
    accumConstr
      [ CEq (CAtom rx1) (CAtom rx3)
      , CEq (CAtom rx3) (CAtom qx)
      ]

    -- r_{(\vec{0}, 1, 0, 0)} = r_{(\vec{0}, 0, 1, 0)} = q_{m+1}
    let vec0 = replicate m 0
    let vr1 = VecIdx . V.fromList $ vec0 ++ [1, 0, 0]
    let vr2 = VecIdx . V.fromList $ vec0 ++ [0, 1, 0]
    r0100 <- coeff r' vr1
    r0010 <- coeff r' vr2
    qm1 <- coeff q $ RankIdx (m + 1)
    accumConstr
      [ CEq (CAtom r0100) (CAtom r0010)
      , CEq (CAtom r0010) (CAtom qm1)
      ]

    -- recursive calls
    q1' <- prove dispatch p  e1
    q2' <- prove dispatch r' e2

    -- equate "return" contexts
    q' <- if Ctx.length q1' > 0
            then returnCtx u
            else emptyCtx u
    eqCtx q1' q2'
    eqCtx q2' q'

    -- TODO: fix expression
    conclude q (TyMatch (TyVar x, τx) ((PNil, (hole, tyHole)) :| [(PNode x1 x2 x3, (hole, tyHole))])) q'
