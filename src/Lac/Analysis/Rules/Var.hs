{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Var where

import           Lac.Analysis.Rules.Common

ruleVar :: Ctx -> Text -> Gen ProofTree
ruleVar q x =
  do
    setRuleName "var"

    assert (numVarsCtx q == 1) "ruleVar: context must have length 1"

    -- TODO: check for variable

    let u = Bound 1
    ((_, ty), _) <- splitCtx' u q x

    if ty == tyTree
      then do
        q' <- returnCtx u

        -- preserve potential
        q1 <- coeff q (IdIdx x)
        q'x <- coeff q' AstIdx
        accumConstr [ CEq (CAtom q1) (CAtom q'x) ]

        conclude q (TyVar x) q'
      else
        conclude q (TyVar x) q
