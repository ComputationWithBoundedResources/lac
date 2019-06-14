module Lac.Analysis.Rules.Ite where

import           Lac.Analysis.Rules.Common

ruleIte :: Rule -> Ctx -> Text -> Typed -> Typed -> Gen Ctx
ruleIte dispatch q x e1 e2 =
  do
    (_, q1) <- splitCtx (Bound 1) q [x]

    qe1 <- dispatch q1 e1
    qe2 <- dispatch q1 e2

    r <- returnCtx (Bound 1)
    eqReturnCtx qe1 qe2
    eqReturnCtx qe2 r

    return r
