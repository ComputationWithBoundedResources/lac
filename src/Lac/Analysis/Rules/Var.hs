{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Var where

import           Lac.Analysis.Rules.Common

ruleVar :: Ctx -> Text -> Gen Ctx
ruleVar q _ =
  do
    assert (numVarsCtx q == 1) "ruleVar: context must have length 1"
    return q
