{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Error where

import           Lac.Analysis.Rules.Common
import           Lac.PP

ruleError :: Ctx -> (Typed, Type) -> Error -> Gen ProofTree
ruleError q tyExpr error =
  return $
    ProofTree
      (q, tyExpr, q)
      (RuleName "error")
      []
      []
      (Just . tshow $ error)
