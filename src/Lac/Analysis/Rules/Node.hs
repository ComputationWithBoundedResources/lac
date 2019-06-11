{-# LANGUAGE RecordWildCards #-}

module Lac.Analysis.Rules.Node where

import           Data.Bound
import           Data.Expr.Typed
import           Data.Type
import           Lac.Analysis.Types

ruleNode :: Ctx -> Typed -> Typed -> Gen Ctx
ruleNode ctx@Ctx{..} (TyVar x1) (TyVar x2) =
  do
    ctx' <- freshCtx >>= augmentCtx (Bound 2) [(x1, tyTree), (x2, tyTree)] True

    q1 <- coeff ctx (IdIdx x1)
    q2 <- coeff ctx (IdIdx x2)
    qx' <- coeff ctx' AstIdx
    q100 <- coeff ctx (VecIdx [1, 0, 0])
    q010 <- coeff ctx (VecIdx [0, 1, 0])
    -- q(a,a,b), q(a,b)

    tellConstr
      [ CEq q1 q2
      , CEq q2 qx'
      , CEq q100 q010
        -- TODO
      ]

    return ctx'
