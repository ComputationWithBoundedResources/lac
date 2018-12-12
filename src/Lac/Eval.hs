{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Eval where

import           Data.Expr.Pretty
import           Data.Expr.Types

import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import Debug.Trace

data Value
  = VClosure Text Expr Env
  | VNat Int
  | VBool Bool
  | VTree TreeValue
  deriving (Eq, Show)

data TreeValue
  = VNil
  | VNode TreeValue Value TreeValue
  deriving (Eq, Show)

class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr TreeValue where
  toExpr =
    \case
      VNil        -> L LNil
      VNode l x r -> L (LNode (toExpr l) (toExpr x) (toExpr r))

instance ToExpr Value where
  toExpr =
    \case
      VNat n         -> L (LNat n)
      VBool a        -> L (LBool a)
      VTree t        -> toExpr t
      VClosure x e _ -> Abs x e -- TODO: env

type Env = Map Text Value

nullEnv :: Env
nullEnv = M.empty

eval :: Env -> Expr -> Value
eval env =
  \case
    L (LBool b) -> VBool b

    L (LNat n) -> VNat n

    L LNil -> VTree VNil
    L (LNode l x r) ->
      let VTree l' = eval env l
          VTree r' = eval env r
          x' = eval env x
      in
      VTree (VNode l' x' r')

    Var x ->
      case M.lookup x (traceShowId env) of
        Just v -> v
        Nothing -> error $ "`" <> T.unpack x <> "` is not in scope"

    Abs x e ->
      VClosure x e env

    App e1 e2 ->
      let t = eval env e1
          u = eval env e2
      in
      case t of
        VClosure x e s ->
          let env' = M.insert x u s
          in
          eval env' e
        _ -> error "expected LHS to be abstraction"

    Let x e1 e2 ->
      eval env (App (Abs x e2) e1)

    Ite e1 e2 e3 ->
      let VBool p = eval env e1
          u = eval env e2
          v = eval env e3
      in
      if p then u else v

    Cmp op e1 e2 ->
      let t = eval env e1
          u = eval env e2
          c = cmp t u
          ok = [(CmpLt, LT), (CmpEq, EQ), (CmpGt, GT)]
      in
      VBool $ (op, c) `elem` ok

    Match e1 cs ->
      let v = eval env e1
      in
      match v (NE.toList cs)

  where
    match :: Value -> [(Pattern, Expr)] -> Value
    match e [] = error $ "match: " <> T.unpack (pretty . toExpr $ e) -- TODO: run-time exception
    match (VTree VNil) ((PNil, e) : _) = eval env e
    match (VTree (VNode l x r)) ((PNode x1 x2 x3, e) : _) =
      let env' = M.insert x1 (VTree l)
               . M.insert x2 x
               . M.insert x3 (VTree r)
               $ env
      in
      eval env' e
    match l (_ : xs) = match l xs

cmp :: Value -> Value -> Ordering
cmp (VNat n)  (VNat m)  = compare n m
cmp (VBool a) (VBool b) = compare a b
cmp _ _ = error "cmp: values cannot be compared"
