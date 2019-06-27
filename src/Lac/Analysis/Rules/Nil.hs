{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Nil where

import           Lac.Analysis.Rules.Common

ruleNil :: Ctx -> Gen Ctx
ruleNil ctx =
  do
    assert (ctxEmpty ctx) $ "ruleNil: context not empty"

    ctx' <- returnCtx (Bound 1)

    forM_ (zip [1..] (enumRankCoeffs ctx)) $ \(i, (_, qc)) -> do
      let splits = [(x, i - x) | x <- [0..i]]
      qab's <- forM splits $ \(a, b) -> coeff ctx' (VecIdx [a, b])
      tellConstr [CEq (CAtom qc) (CSum (map CAtom qab's))]

    return ctx'
