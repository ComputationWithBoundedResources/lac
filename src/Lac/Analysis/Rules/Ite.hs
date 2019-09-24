{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Ite where

import           Lac.Analysis.Rules.Common

ruleIte
  :: Rule
  -> Ctx
  -> Text
  -> (Typed, Type)
  -> (Typed, Type)
  -> Gen ProofTree
ruleIte dispatch q x (e1, τe1) (e2, τe2) =
  do
    assert (τe1 == τe2) "ruleIte: e1 and e2 have distinct types"

    setRuleName "ite"

    (_, q1) <- splitCtx def q [x]

    q1 <- prove dispatch q1 (e1, τe1)
    q2 <- prove dispatch q1 (e2, τe2)

    q' <- returnCtx def
    eqCtx q1 q2
    eqCtx q2 q'

    conclude
      q
      (TyIte (TyVar x, tyBool) (hole, tyBool) (hole, tyBool), τe1)
      q'
