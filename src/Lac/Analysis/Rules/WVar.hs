{-# LANGUAGE LambdaCase #-}

module Lac.Analysis.Rules.WVar where

import           Lac.Analysis.Rules.Common

import qualified Data.Text.IO              as T (putStrLn)

ruleWVar :: Rule -> Ctx -> Text -> Gen ProofTree
ruleWVar dispatch q x =
  do
    setRuleName "w : var"

    let u = Bound 1
    (_, r) <- weakenCtx u q [x]

    -- equate rank coefficients
    forM_ (coeffs r isRankCoeff) $ \(idx, ri) -> do
      qi <- coeff q idx
      accumConstr [CEq (CAtom qi) (CAtom ri)]

    -- equate vector coefficients
    let rabs = vecCoeffsRev r $
                  \case b:as -> True
                        _    -> False
    forM_ rabs $ \(VecIdx vec, rab) -> do
                  let as = init vec
                      b = last vec
                  qa0b <- coeff q (VecIdx (as ++ [0, b]))
                  accumConstr [CEq (CAtom rab) (CAtom qa0b)]

    q' <- prove dispatch r (TyVar x)

    conclude q (TyVar x) q'
