{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Nil where

import           Lac.Analysis.Rules.Common

import qualified Data.List.Ext             as L
import qualified Data.Vector               as V

ruleNil :: Ctx -> Typed -> Gen ProofTree
ruleNil q e =
  do
    setRuleName "nil"

    let (Bound u) = def

    assert (ctxEmpty q) $ "ruleNil: context not empty"

    q' <- returnCtx def

    -- q_{(c)} = \sum_{a+b=c} q'_{(a,b)}
    forM_ [0..u] $ \c -> do
      qc <- coeff q $ VecIdx . V.fromList $ [c]
      forM_ (L.split c) $ \(a, b) -> do
        q'ab <- coeff q' $ VecIdx . V.fromList $ [a, b]
        accumConstr [ CEq (CAtom qc) (CAtom q'ab) ]

    conclude q nil q'
