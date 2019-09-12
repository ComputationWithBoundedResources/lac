module Lac.Analysis.Rules.Nat where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

ruleNat :: Ctx -> Typed -> Gen ProofTree
ruleNat q e@(TyLit (TyLNat _)) =
  do
    setRuleName "nat"
    assert
      (Ctx.length q == 0)
      "ruleNat: context empty"
    conclude q e q
