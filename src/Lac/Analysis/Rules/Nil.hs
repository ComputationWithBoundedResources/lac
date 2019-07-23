{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Nil where

import           Lac.Analysis.Rules.Common

ruleNil :: Ctx -> Typed -> Gen ProofTree
ruleNil q e =
  do
    setRuleName "nil"

    assert (ctxEmpty q) $ "ruleNil: context not empty"

    q' <- returnCtx (Bound 1)

    -- TODO: constraints

    conclude q nil q'
