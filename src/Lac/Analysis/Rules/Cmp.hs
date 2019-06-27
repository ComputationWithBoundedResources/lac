module Lac.Analysis.Rules.Cmp where

import           Lac.Analysis.Rules.Common

ruleCmp :: Ctx -> Text -> Text -> Gen Ctx
ruleCmp q x1 x2 =
  do
    let u = Bound 1
    (_, r) <- splitCtx u q [x1, x2]
    assert (ctxEmpty r) $ "ruleCmp: Q - { " <> x1 <> ", " <> x2 <> " } not empty"
    return r
