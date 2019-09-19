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
import           Data.Type                (isTyTree)
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Rules.App   as E
import           Lac.Analysis.Rules.Bool  as E
import           Lac.Analysis.Rules.Cmp   as E
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

dispatchReport :: Ctx -> Typed -> Gen ProofTree
dispatchReport context expression = dispatch context expression `catchError` handler
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

dispatch :: Ctx -> Typed -> Gen ProofTree
dispatch q e =
  case e of
    _ | (z:_) <- nonLinear q e ->
      ruleShift (ruleShare dispatch z) (pushBack q [z]) q e
    TyMatch (TyVar x, _) ((PNil, (e1, _)) :| [(PNode x1 x2 x3, (e2, _))]) ->
      let ruleMatch' q' _ = ruleMatch dispatch q' x e1 (x1, x2, x3) e2
      in
      ruleShift ruleMatch' (pushBack q [x]) q e
    TyLit (TyLNat _) ->
      if numVarsCtx q == 0
        then ruleW ruleNat q e
        else ruleWVar dispatch q e []
    TyLit (TyLBool _) ->
      if numVarsCtx q == 0
        then ruleW ruleBool q e
        else ruleWVar dispatch q e []
    TyLit TyLNil ->
      if numVarsCtx q == 0
        then ruleW ruleNil q e
        else ruleWVar dispatch q e []
    TyLit (TyLNode (TyVar x1) (TyVar x2) (TyVar x3)) ->
      if numVarsCtx q > 3
        then ruleWVar dispatch q e [x1, x2, x3]
        else ruleW ruleNode q e
    TyVar x ->
      if numVarsCtx q == 1
        then ruleW ruleVar q e
        else ruleWVar dispatch q e [x]
    TyCmp _ (TyVar x1, _) (TyVar x2, _) ->
      if numVarsCtx q == 2
        then ruleW ruleCmp q e
        else ruleWVar dispatch q e [x1, x2]
    TyIte (TyVar x, _) (e1, _) (e2, _) ->
      ruleIte dispatch q x e1 e2
    TyLet _ (e1, _) _ ->
      ruleShift (ruleLet dispatch) (letOrder q e1) q e
    TyApp _ _ ->
      ruleApp dispatch q e
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
