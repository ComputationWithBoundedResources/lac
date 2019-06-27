{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Var where

import           Lac.Analysis.Rules.Common

ruleVar :: Ctx -> Text -> Gen Ctx
ruleVar q _ =
  do
    assert (ctxEmpty q) "ruleVar: context not empty"
    return q
