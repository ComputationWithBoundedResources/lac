{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Bool where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

ruleBool :: Ctx -> Typed -> Gen ProofTree
ruleBool q e@(TyLit (TyLBool _)) =
  do
    setRuleName "bool"
    assert
      (Ctx.length q == 0)
      "ruleBool: context empty"
    conclude q e q
