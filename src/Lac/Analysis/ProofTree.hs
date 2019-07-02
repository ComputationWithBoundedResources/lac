{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.ProofTree where

import           Data.Expr.Typed
import           Lac.Analysis.RuleName
import           Lac.Analysis.Types

data ProofTree
  = ProofTree (Ctx, Typed, Ctx) [(RuleName, [Constraint], [ProofTree])]
  deriving (Show)
