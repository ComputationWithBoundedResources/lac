{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Rules.Node where

import           Lac.Analysis.Rules.Common

ruleNode :: Ctx -> Typed -> Gen ProofTree
ruleNode ctx (TyLit (TyLNode (TyVar x1) (TyVar _) (TyVar x2))) =
  do
    setRuleName "node"

    ctx' <- returnCtx def

    q1 <- coeff ctx (RankIdx 1)
    q2 <- coeff ctx (RankIdx 2)
    qx' <- coeff ctx' astIdx

    -- TODO: q100 = q010 = qx'

    -- TODO: q(a,a,b), q(a,b)

    accumConstr $
      [ CEq (CAtom q1) (CAtom q2)
      , CEq (CAtom q2) (CAtom qx')
      ]

    conclude ctx (TyLit (TyLNode (TyVar x1) hole (TyVar x2))) ctx'
