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
import           Lac.Analysis.Rules.Let   as E
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
    TyMatch (TyVar x, _) ((PNil, (e1, _)) :| [(PNode x1 x2 x3, (e2, _))]) ->
      ruleMatch dispatch q x e1 (x1, x2, x3) e2
    TyLit TyLNil ->
      if numVarsCtx q == 0
        then ruleNil q e
        else ruleWVar dispatch q e []
    TyLit (TyLNode (TyVar x1) (TyVar x2) (TyVar x3)) ->
      if numVarsCtx q > 3
        then ruleWVar dispatch q e [x1, x2, x3]
        else ruleNode q x1 x3
    TyVar x ->
      if numVarsCtx q == 1
        then ruleVar q x
        else ruleWVar dispatch q e [x]
    TyCmp _ (TyVar x1, _) (TyVar x2, _) ->
      if numVarsCtx q == 2
        then ruleCmp q e
        else ruleWVar dispatch q e [x1, x2]
    TyIte (TyVar x, _) (e1, _) (e2, _) ->
      ruleIte dispatch q x e1 e2
    TyLet _ _ _ ->
      -- TODO: apply (share) rule if context is not linear
      ruleLet dispatch q e
    _ -> throwError (AssertionFailed "dispatch: rule unimplemented")
