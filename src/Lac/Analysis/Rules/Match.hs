{-# LANGUAGE LambdaCase #-}

module Lac.Analysis.Rules.Match where

import           Lac.Analysis.Rules.Common

import qualified Data.Text.IO              as T

ruleMatch :: Ctx -> Text -> Typed -> (Text, Text, Text) -> Typed -> Gen Ctx
ruleMatch q x e1 (x1, x2, x3) e2 =
  do
    let u = Bound 1

    (_, p) <- splitCtx u q [x]
    liftIO $ do
      putStrLn "P"
      T.putStrLn (ppCtx p)

    (_, r) <- splitCtx u q [x]
    r' <- augmentCtx u r [(x1, tyTree), (x2, tyNat), (x3, tyTree)]
    liftIO $ do
      putStrLn "R"
      T.putStrLn (ppCtx r')

    -- r(vec{a}, a, a, b) = q(vec{a}, a, b)
    let raaabs = vecCoeffsRev r' $
                  \case
                    (b:a1:a2:as) | a1 == a2, all (== a1) as -> True
                    _ -> False
    qaabs <- forM raaabs $ \(VecIdx xs, _) ->
                                              let a = head xs
                                                  b = last xs
                                                  vec = replicate (length xs - 2) a ++ [b]
                                              in
                                              coeff q (VecIdx vec)

    forM (zip (map snd raaabs) qaabs) $ \(ri, qi) -> tellConstr [CEq (CAtom ri) (CAtom qi)]

    return q
