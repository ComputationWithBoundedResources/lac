module Lac.Analysis.Rules.Var where

import           Lac.Analysis.Rules.Common

ruleVar :: Ctx -> Text -> Gen Ctx
ruleVar q _ = return q
