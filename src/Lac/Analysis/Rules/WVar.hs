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

    -- equate rank coefficients
    forM_ (coeffs r isRankCoeff) $ \(idx, ri) -> do
      qi <- coeff q idx
      accumConstr [CEq (CAtom qi) (CAtom ri)]

    -- TODO: equate vector coefficients

    q' <- prove dispatch r e

    conclude q e q'
