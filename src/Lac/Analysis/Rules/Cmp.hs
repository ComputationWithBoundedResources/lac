module Lac.Analysis.Rules.Cmp where

import           Lac.Analysis.Rules.Common

ruleCmp :: Ctx -> Text -> Text -> Gen Ctx
ruleCmp q _ _ = return q
