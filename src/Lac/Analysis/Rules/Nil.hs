{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Nil where

import           Lac.Analysis.Rules.Common

ruleNil :: Ctx -> Typed -> Gen ProofTree
ruleNil q e =
  do
    setRuleName "nil"

    assert (ctxEmpty q) $ "ruleNil: context not empty"

    q' <- returnCtx (Bound 1)

    -- q_{(c)} = \sum_{a+b=c} q'_{(a,b)}
    forVec_ q selAll $ \xs ->
      case xs of
        ([(x, c)], qc) | x == costId ->
          forVec_ q' selAll $ \ys ->
            case ys of
              ([(x, a), (y, b)], qab) | a + b == c ->
                accumConstr [ CEq (CAtom qc) (CAtom qab) ]
              _ -> return ()
        _ -> throwError $ AssertionFailed "ruleNil: found unexpected non-cost coefficient in context"

    conclude q nil q'
