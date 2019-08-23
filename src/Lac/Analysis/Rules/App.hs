{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.App where

import           Lac.Analysis.Rules.Common

ruleApp :: Rule -> Ctx -> Typed -> Gen ProofTree
ruleApp _ q1 e =
  do
    setRuleName "app"

    -- TODO: lookup annotation

    q <- copyCtx q1

    let needle = vecIdx $ zip (trees q) (repeat 0) ++ [("+", 2)]
    let lu = filter ((== needle) . fst) . coeffs

    -- qi ∈ Q, q1i ∈ Q + 1
    case (lu q, lu q1) of
      ([(_, qi)], [(_, q1i)]) ->
        accumConstr [ CEq (CAtom q1i) (CSum [CAtom qi, CInt 1]) ]
      _ ->
        throwError $ AssertionFailed "ruleApp"

    forM_ (filter ((/= needle) . fst) $ coeffs q) $ \(i, qi) -> do
      q1i <- coeff q1 i
      accumConstr [ CEq (CAtom qi) (CAtom q1i) ]

    q' <- returnCtx def

    conclude q1 e q'
