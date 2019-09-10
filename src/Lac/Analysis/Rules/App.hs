{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.App where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import           Control.Monad.Ext         (ifM)
import qualified Data.Vector               as V

ruleApp :: Rule -> Ctx -> Typed -> Gen ProofTree
ruleApp _ q1 e =
  do
    setRuleName "app"

    -- TODO: lookup annotation

    let m = Ctx.length q1
    q <- copyCtx q1

    let v = VecIdx . V.fromList $ replicate m 0 ++ [2]

    q1v <- coeff q1 v
    qv  <- coeff q  v

    cost <- ifM isCostFree (return 0) (return 1)

    accumConstr [ CEq (CAtom q1v) (CSum [CAtom qv, CInt cost]) ]

    forM_ (filter ((/= v) . fst) $ coeffs q) $ \(i, qi) -> do
      q1i <- coeff q1 i
      accumConstr [ CEq (CAtom qi) (CAtom q1i) ]

    q' <- returnCtx def

    conclude q1 e q'
