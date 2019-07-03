{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Var where

import           Lac.Analysis.Rules.Common

ruleVar :: Ctx -> Text -> Gen ProofTree
ruleVar q x =
  do
    setRuleName "var"
    assert (numVarsCtx q == 1) "ruleVar: context must have length 1"
    -- TODO: check for variable
    conclude q (TyVar x) q
