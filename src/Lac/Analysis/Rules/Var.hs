{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Var where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import           Debug.Trace

ruleVar :: Ctx -> Typed -> Gen ProofTree
ruleVar q (TyVar x) =
  do
    setRuleName "var"

    let u = def
    ((_, ty), _) <- splitCtx' u q x

    if ty == tyTree
      then do
        assert
          (Ctx.length q == 1)
          "ruleVar: context must consist of exactly one tree"

        q' <- returnCtx u

        -- preserve potential
        q1 <- coeff q (RankIdx 1)
        q'x <- coeff q' astIdx
        accumConstr [ CEq (CAtom q1) (CAtom q'x) ]

        conclude q (TyVar x) q'
      else do
        assert
          (Ctx.length q == 0)
          "ruleVar: context must not contain trees"

        conclude q (TyVar x) q
