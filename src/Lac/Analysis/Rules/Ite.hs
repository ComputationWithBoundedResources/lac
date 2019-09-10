{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Ite where

import           Lac.Analysis.Rules.Common

ruleIte :: Rule -> Ctx -> Text -> Typed -> Typed -> Gen ProofTree
ruleIte dispatch q x e1 e2 =
  do
    setRuleName "ite"

    (_, q1) <- splitCtx def q [x]

    q1 <- prove dispatch q1 e1
    q2 <- prove dispatch q1 e2

    q' <- returnCtx def
    eqCtx q1 q2
    eqCtx q2 q'

    conclude
      q
      (TyIte (TyVar x, tyBool) (hole, tyBool) (hole, tyBool))
      q'
