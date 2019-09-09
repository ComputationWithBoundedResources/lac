{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Cmp where

import           Lac.Analysis.Rules.Common

ruleCmp :: Ctx -> Typed -> Gen ProofTree
ruleCmp q e@(TyCmp op (TyVar x1, τ1) (TyVar x2, τ2)) =
  do
    setRuleName "cmp"

    let u = def

    forM_ [(x1, τ1), (x2, τ2)] $ \(x, τ) ->
      assert
        (τ == tyNat)
        ("ruleCmp: " <> x <> " must have type Nat")

    (_, r) <- splitCtx u q [x1, x2]

    assert
      (ctxEmpty r)
      ("ruleCmp: Q - { " <> x1 <> ", " <> x2 <> " } not empty")

    conclude q e r
