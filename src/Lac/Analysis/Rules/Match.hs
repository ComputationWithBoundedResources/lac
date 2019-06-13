module Lac.Analysis.Rules.Match where

import           Lac.Analysis.Rules.Common

import qualified Data.Text.IO              as T

ruleMatch :: Ctx -> Text -> Typed -> (Text, Text, Text) -> Typed -> Gen Ctx
ruleMatch ctx x e1 (x1, x2, x3) e2 =
  do
    let u = Bound 1

    (_, p) <- splitCtx u ctx [x]
    liftIO $ do
      putStrLn "P"
      T.putStrLn (ppCtx p)

    (_, r) <- splitCtx u ctx [x]
    r' <- augmentCtx u r [(x1, tyTree), (x2, tyNat), (x3, tyTree)]
    liftIO $ do
      putStrLn "R"
      T.putStrLn (ppCtx r')

    return ctx
