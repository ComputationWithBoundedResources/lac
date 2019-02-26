{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      hiding (expr)
import           Lac.Analysis.Types
import           Lac.TypeInference

import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Text                      (Text)
import qualified Data.Text                      as T

type Rule a = Ctx -> Expr -> Ctx -> Gen a

dispatch :: Rule ()
dispatch ctx expr ctx' = do
  gen <- case expr of
    Abs _ _   -> throwError $ NotImplemented "abstraction"
    Lit _     -> return genLit
    Cmp _ _ _ -> throwError $ NotImplemented "comparison"
    Ite _ _ _ -> throwError $ NotImplemented "if-then-else"
    Let _ _ _ -> throwError $ NotImplemented "let"
    App _ _   -> throwError $ NotImplemented "application"
    Match _ _ -> return genMatch
    Var _     -> return genVar
  gen ctx expr ctx'

genVar :: Rule ()
-- TODO: compare contexts
genVar _ (Var _) _ = return ()
genVar _ _ _ = throwError $ NotApplicable "variable"

genLit :: Rule ()
-- TODO: apply weakening
genLit ctx (Lit LNil) ctxR = return ()
genLit _ _ _ = throwError $ NotApplicable "literal"

genMatch :: Rule ()
genMatch ctx (Match (Var x) ((PNil, e1) :| [(PNode x1 x2 x3, e2)])) ctxR =
  do
    (ty, ctx'@Ctx{..}) <- splitCtx x ctx
    -- TODO: check type
    liftIO $ print ty
    dispatch ctx' e1 ctxR
    -- TODO: use fresh type variable instead of abstract type here?
    ctx'' <- augmentCtx [ (x1, AnTy (tyTree tyAbs) ())
                        , (x2, AnTy tyAbs          ())
                        , (x3, AnTy (tyTree tyAbs) ())
                        ] ctx
    dispatch ctx'' e2 ctxR
    return ()
genMatch _ _ _ =
  throwError $ NotApplicable "match"
