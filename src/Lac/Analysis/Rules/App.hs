{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.App where

import           Data.Expr.FromTyped       (fromTyped)
import           Data.Expr.Types           (unApp)
import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import           Control.Monad.Ext         (ifM)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Vector               as V

import           Debug.Trace

ruleApp :: Rule -> Ctx -> Typed -> Gen ProofTree
ruleApp _ q1 e =
  do
    setRuleName "app"

    case unApp (fromTyped e) of
      Just (f :| xs) ->
        go f xs
      Nothing ->
        throwError . AssertionFailed $ "ruleApp: ill-formed expression"
 where
  go f xs = do
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
