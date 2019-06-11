{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Lac.Analysis.Rules.Node where

import           Data.Bound
import           Data.Expr.Typed
import           Data.Type
import           Lac.Analysis.Types

import           Control.Monad      (forM)
import           Data.Text          (Text)

ruleNode :: Ctx -> Text -> Text -> Gen Ctx
ruleNode ctx@Ctx{..} x1 x2 =
  do
    ctx' <- returnCtx (Bound 1) 1 True

    q1 <- coeff ctx (IdIdx x1)
    q2 <- coeff ctx (IdIdx x2)
    qx' <- coeff ctx' AstIdx
    q100 <- coeff ctx (VecIdx [1, 0, 0])
    q010 <- coeff ctx (VecIdx [0, 1, 0])
    -- q(a,a,b), q(a,b)
    let qaabs = coeffs ctx $ \case
                               VecIdx [a, a', b] | a == a' -> True
                               _ -> False
    qabs <- forM qaabs $ \(VecIdx [a, _, b], _) -> coeff ctx' (VecIdx [a, b])

    tellConstr $
      [ CEq q1 q2
      , CEq q2 qx'
      , CEq q100 q010
      , CEq q010 qx'
      ]
      ++
      zipWith CEq (map snd qaabs) qabs

    return ctx'
