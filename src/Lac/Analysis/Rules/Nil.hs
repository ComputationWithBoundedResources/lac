{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Nil where

import           Lac.Analysis.Rules.Common

ruleNil :: Ctx -> Typed -> Gen ProofTree
ruleNil q e =
  do
    setRuleName "nil"

    assert (ctxEmpty q) $ "ruleNil: context not empty"

    q' <- returnCtx (Bound 1)

    forM_ (zip [1..] (enumRankCoeffs q)) $ \(i, (_, qc)) -> do
      let splits = [(x, i - x) | x <- [0..i]]
      qab's <- forM splits $ \(a, b) -> coeff q' (VecIdx [a, b])
      accumConstr [CEq (CAtom qc) (CSum (map CAtom qab's))]

    conclude q nil q'
