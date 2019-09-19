module Lac.Analysis.Rules.Error where

import           Lac.Analysis.Rules.Common
import           Lac.PP

ruleError :: Ctx -> Typed -> Error -> Gen ProofTree
ruleError q e error =
  return $
    ProofTree
      (q, e, q)
      (RuleName "error")
      []
      []
      (Just . tshow $ error)
