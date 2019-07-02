{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules (
    module E
  , dispatch
  ) where

import           Data.Expr.Typed
import           Data.Expr.Types
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Rules.Cmp   as E
import           Lac.Analysis.Rules.Ite   as E
import           Lac.Analysis.Rules.Match as E
import           Lac.Analysis.Rules.Nil   as E
import           Lac.Analysis.Rules.Node  as E
import           Lac.Analysis.Rules.Var   as E
import           Lac.Analysis.Rules.WVar  as E
import           Lac.Analysis.Types

import           Data.List.NonEmpty       (NonEmpty (..))

dispatch :: Ctx -> Typed -> Gen ProofTree
dispatch q e =
  case e of
    {-
    TyMatch (TyVar x, _) ((PNil, (e1, _)) :| [(PNode x1 x2 x3, (e2, _))]) ->
      ruleMatch dispatch q x e1 (x1, x2, x3) e2
    -}
    TyLit TyLNil ->
      ruleNil q
    TyLit (TyLNode (TyVar x1) _ (TyVar x2)) ->
      ruleNode q x1 x2
    {-
    TyVar x ->
      if ctxEmpty q
        then ruleVar q x
        else ruleWVar dispatch q x
    TyCmp _ (TyVar x1, _) (TyVar x2, _) ->
      ruleCmp q x1 x2
    TyIte (TyVar x, _) (e1, _) (e2, _) ->
      ruleIte dispatch q x e1 e2
    -}
    _ -> throwError (AssertionFailed "dispatch: rule unimplemented")
