{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Rules (
    module E
  , dispatch
  , dispatchReport
  ) where

import           Data.Expr.FromTyped
import           Data.Expr.Typed
import           Data.Expr.Types
import           Data.Type                (Type, isTyTree)
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Rules.App   as E
import           Lac.Analysis.Rules.Bool  as E
import           Lac.Analysis.Rules.Cmp   as E
import           Lac.Analysis.Rules.Error as E
import           Lac.Analysis.Rules.Ite   as E
import           Lac.Analysis.Rules.Let   as E
import           Lac.Analysis.Rules.Match as E
import           Lac.Analysis.Rules.Nat   as E
import           Lac.Analysis.Rules.Nil   as E
import           Lac.Analysis.Rules.Node  as E
import           Lac.Analysis.Rules.Share as E
import           Lac.Analysis.Rules.Shift as E
import           Lac.Analysis.Rules.Var   as E
import           Lac.Analysis.Rules.W     as E
import           Lac.Analysis.Rules.WVar  as E
import           Lac.Analysis.Types
import           Lac.Analysis.Types.Ctx
import           Lac.PP.Pretty

import           Control.Monad.Except     (catchError)
import           Data.List.Ext            (elemElem)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

import           Debug.Trace

dispatchReport :: Ctx -> (Typed, Type) -> Gen ProofTree
dispatchReport context typedExpression@(expression, τ) =
    dispatch context typedExpression `catchError` handler
  where
    handler :: Error -> Gen ProofTree
    handler error = do
      liftIO . T.putStrLn . T.unlines $
        [ ""
        , "Expression:\n"
        , "  " <> (pretty . fromTyped $ expression)
        , ""
        ]
      throwError error

dispatch :: Ctx -> (Typed, Type) -> Gen ProofTree
dispatch = catch _dispatch

catch :: Rule -> Ctx -> (Typed, Type) -> Gen ProofTree
catch f q@Ctx{..} tyExpr@(e, τ) = f q tyExpr `catchError` handler
  where
    handler error = do
      liftIO $ do
        print ctxVariables
        T.putStrLn . pretty . fromTyped $ e
        print error
      ruleError q e error

_dispatch :: Ctx -> (Typed, Type) -> Gen ProofTree
_dispatch q tyExpr@(e, τ) =
  case e of
    _ | (z:_) <- nonLinear q e ->
      ruleShift (ruleShare dispatch z) (pushBack q [z]) q tyExpr
    TyMatch (TyVar x, _) ((PNil, tyExpr1) :| [(PNode x1 x2 x3, tyExpr2)]) ->
      let ruleMatch' q' _ = ruleMatch dispatch q' x tyExpr1 (x1, x2, x3) tyExpr2
      in
      ruleShift ruleMatch' (pushBack q [x]) q tyExpr
    TyLit (TyLNat _) ->
      if numVarsCtx q == 0
        then ruleW ruleNat q tyExpr
        else ruleWVar dispatch q tyExpr []
    TyLit (TyLBool _) ->
      if numVarsCtx q == 0
        then ruleW ruleBool q tyExpr
        else ruleWVar dispatch q tyExpr []
    TyLit TyLNil ->
      if numVarsCtx q == 0
        then ruleW ruleNil q tyExpr
        else ruleWVar dispatch q tyExpr []
    TyLit (TyLNode (TyVar x1) (TyVar x2) (TyVar x3)) ->
      if and [ numVarsCtx q == 3
             , x1 `elem` trees q
             , x2 `elem` nats q
             , x3 `elem` trees q
             ]
        then ruleW ruleNode q tyExpr
        else ruleWVar dispatch q tyExpr [x1, x2, x3]
    TyVar x ->
      if numVarsCtx q == 1
        then ruleW ruleVar q tyExpr
        else ruleWVar dispatch q tyExpr [x]
    TyCmp _ (TyVar x1, _) (TyVar x2, _) ->
      if numVarsCtx q == 2
        then ruleW ruleCmp q tyExpr
        else ruleWVar dispatch q tyExpr [x1, x2]
    TyIte (TyVar x, _) tyExpr1 tyExpr2 ->
      ruleIte dispatch q x tyExpr1 tyExpr2
    TyLet _ (e1, _) _ ->
      ruleShift (ruleLet dispatch) (letOrder q e1) q tyExpr
    TyApp _ _ ->
      ruleApp dispatch q tyExpr
    _ ->
      throwError (AssertionFailed "dispatch: rule unimplemented")

-- | Find non-linear variables, i.e. variables that are used twice in an
-- expression.
nonLinear :: Ctx -> Typed -> [Text]
nonLinear Ctx{..} e =
    filter (`elemElem` xs) ts
  where
    xs = var' .  fromTyped $ e
    ts = map fst . filter (isTyTree . snd) $ ctxVariables
