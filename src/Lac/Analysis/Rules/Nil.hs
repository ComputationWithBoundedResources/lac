{-# LANGUAGE LambdaCase #-}

module Lac.Analysis.Rules.Nil where

import           Data.Bound
import           Data.Expr.Typed
import           Data.Type
import           Lac.Analysis.Types

import           Control.Monad      (forM, forM_)
import           Data.Text          (Text)

ruleNil :: Ctx -> Gen Ctx
ruleNil ctx =
  do
    ctx' <- returnCtx (Bound 1) 1 False

    forM_ (zip [1..] (enumRankCoeffs ctx)) $ \(i, (_, qc)) -> do
      let splits = [(x, i - x) | x <- [0..i]]
      qab's <- forM splits $ \(a, b) -> coeff ctx' (VecIdx [a, b])
      tellConstr [CEq (CAtom qc) (CSum (map CAtom qab's))]

    return ctx'

test =
  runGen $ do
    ctx <- augmentCtx (Bound 1) [("t", tyTree)] False rootCtx
    ruleNil ctx
