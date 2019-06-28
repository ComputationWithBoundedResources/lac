{-# LANGUAGE LambdaCase #-}

module Lac.Analysis.Rules.WVar where

import           Lac.Analysis.Rules.Common

ruleWVar :: Rule -> Ctx -> Text -> Gen Ctx
ruleWVar dispatch q x =
  do
    let u = Bound 1
    (_, r) <- weakenCtx u q x

    -- equate rank coefficients
    forM_ (coeffs r isRankCoeff) $ \(idx, ri) -> do
      qi <- coeff q idx
      tellConstr [CEq (CAtom qi) (CAtom ri)]

    -- equate vector coefficients
    let rabs = vecCoeffsRev r $
                  \case b:as -> True
                        _    -> False
    forM_ rabs $ \(VecIdx vec, rab) -> do
                  let as = init vec
                      b = last vec
                  qa0b <- coeff q (VecIdx (as ++ [0, b]))
                  tellConstr [CEq (CAtom rab) (CAtom qa0b)]

    dispatch r (TyVar x)
