{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Nil where

import           Lac.Analysis.Rules.Common

import qualified Data.List.Ext             as L

ruleNil :: Ctx -> Typed -> Gen ProofTree
ruleNil q e =
  do
    setRuleName "nil"

    let (Bound u) = def

    assert (ctxEmpty q) $ "ruleNil: context not empty"

    q' <- returnCtx def

    -- q_{(c)} = \sum_{a+b=c} q'_{(a,b)}
    forM_ [0..u] $ \c -> do
      qc <- vecIdx q [(costId, c)] >>= coeff q
      forM_ (L.split c) $ \(a, b) -> do
        q'ab <- vecIdx q' [(astId, a), (costId, b)] >>= coeff q'
        accumConstr [ CEq (CAtom qc) (CAtom q'ab) ]

    conclude q nil q'
