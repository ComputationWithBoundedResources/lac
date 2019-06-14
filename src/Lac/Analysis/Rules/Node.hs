{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Rules.Node where

import           Data.Bound
import           Data.Expr.Typed
import           Data.Type
import           Lac.Analysis.Types

import           Control.Monad      (forM)
import           Data.Text          (Text)

ruleNode :: Ctx -> Text -> Text -> Gen Ctx
ruleNode ctx x1 x2 =
  do
    ctx' <- returnCtx (Bound 1)

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
      [ CEq (CAtom q1) (CAtom q2)
      , CEq (CAtom q2) (CAtom qx')
      , CEq (CAtom q100) (CAtom q010)
      , CEq (CAtom q010) (CAtom qx')
      ]
      ++
      zipWith (\x y -> CEq (CAtom x) (CAtom y)) (map snd qaabs) qabs

    return ctx'
