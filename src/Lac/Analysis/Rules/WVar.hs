{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.WVar where

import           Lac.Analysis.Rules.Common

ruleWVar :: Rule -> Ctx -> Typed -> [Text] -> Gen ProofTree
ruleWVar dispatch q e xs =
  do
    setRuleName "w : var"

    let u = Bound 1
    (_, r) <- weakenCtx u q xs

    -- r_i = q_i
    forM_ (coeffs r isRankCoeff) $ \(idx, ri) -> do
      qi <- coeff q idx
      accumConstr [CEq (CAtom qi) (CAtom ri)]

    -- r_{(\vec{a},b)} = q_{(\vec{a},0,b)}
    forVec_ r (dropCtxVars q) $ \ys ->
      case ys of
        ([(y, b)], rb) | y == costId ->
          forVec_ q (dropCtxVars q) $ \zs ->
            let p (x, a) = ((x `elem` xs) && a == 0) || (x == costId && a == b)
            in
            case zs of
              (as, qb') | all p as ->
                accumConstr [CEq (CAtom rb) (CAtom qb')]
              _ ->
                return ()
        _ -> throwError (AssertionFailed "ruleWVar: illegal vector index")

    q' <- prove dispatch r e

    conclude q e q'
