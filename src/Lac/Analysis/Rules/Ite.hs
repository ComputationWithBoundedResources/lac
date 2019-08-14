{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Ite where

import           Lac.Analysis.Rules.Common

ruleIte :: Rule -> Ctx -> Text -> Typed -> Typed -> Gen ProofTree
ruleIte dispatch q x e1 e2 =
  do
    setRuleName "ite"

    (_, q1) <- splitCtx def q [x]

    qe1 <- prove dispatch q1 e1
    qe2 <- prove dispatch q1 e2

    r <- returnCtx def
    eqCtx qe1 qe2
    eqCtx qe2 r

    conclude
      q
      (TyIte (TyVar x, tyBool) (hole, tyBool) (hole, tyBool))
      r
