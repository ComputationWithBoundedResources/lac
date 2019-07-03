module Lac.Analysis.Rules.Ite where

import           Lac.Analysis.Rules.Common

ruleIte :: Rule -> Ctx -> Text -> Typed -> Typed -> Gen ProofTree
ruleIte dispatch q x e1 e2 =
  do
    (_, q1) <- splitCtx (Bound 1) q [x]

    t1@(ProofTree (_, _, qe1) _ _ _) <- dispatch q1 e1
    t2@(ProofTree (_, _, qe2) _ _ _) <- dispatch q1 e2

    r <- returnCtx (Bound 1)
    eqReturnCtx qe1 qe2
    eqReturnCtx qe2 r

    return $
      ProofTree
        (q, TyIte (TyVar x, tyBool) (hole, tyBool) (hole, tyBool), r)
        (RuleName "ite")
        []
        [t1, t2]
