{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Rules.Node where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import qualified Data.Vector               as V

ruleNode :: Ctx -> Typed -> Gen ProofTree
ruleNode q (TyLit (TyLNode (TyVar x1) (TyVar _) (TyVar x2))) =
  do
    setRuleName "node"

    assert
      (Ctx.length q == 2)
      "ruleNode: context must contain exactly two variables (trees)"

    q' <- returnCtx def

    -- q_1 = q_2 = q'_\ast

    q1 <- coeff q (RankIdx 1)
    q2 <- coeff q (RankIdx 2)
    q'x <- coeff q' astIdx

    accumConstr $
      [ CEq (CAtom q1) (CAtom q2)
      , CEq (CAtom q2) (CAtom q'x)
      ]

    -- q_{(1,0,0)} = q_{(0,1,0)} = q'_\ast

    q100 <- coeff q (VecIdx . V.fromList $ [1, 0, 0])
    q010 <- coeff q (VecIdx . V.fromList $ [0, 1, 0])

    accumConstr $
      [ CEq (CAtom q100) (CAtom q010)
      , CEq (CAtom q010) (CAtom q'x)
      ]

    -- q_{(a,a,b)} = q'_{(a,b)}

    let (Bound u) = def

    forM_ [(a, b) | a <- [0..u], b <- [0..u]] $ \(a, b) -> do
      qaab <- coeff q (VecIdx . V.fromList $ [a, a, b])
      q'ab <- coeff q' (VecIdx . V.fromList $ [a, b])
      accumConstr [ CEq (CAtom qaab) (CAtom q'ab) ]

    conclude q (TyLit (TyLNode (TyVar x1) hole (TyVar x2))) q'
