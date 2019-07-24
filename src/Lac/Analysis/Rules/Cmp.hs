{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Cmp where

import           Lac.Analysis.Rules.Common

ruleCmp :: Ctx -> Typed -> Gen ProofTree
ruleCmp q e@(TyCmp op (TyVar x1, _) (TyVar x2, _)) =
  do
    setRuleName "cmp"

    let u = Bound 1

    (_, r) <- splitCtx u q [x1, x2]
    assert (ctxEmpty r) $ "ruleCmp: Q - { " <> x1 <> ", " <> x2 <> " } not empty"

    conclude q e r
