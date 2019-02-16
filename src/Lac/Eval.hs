{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Eval where

import           Data.Expr.Pretty
import           Data.Expr.Types
import           Data.Expr.Types as Expr (fromDecl)
import           Lac.Eval.Value

import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T

fromDecl :: [Text] -> Expr -> Value
fromDecl (x:xs) e = VClosure x (Expr.fromDecl xs e) nullEnv
fromDecl []     e = eval nullEnv nullEnv e

eval :: Env -> Env -> Expr -> Value
eval global env =
  \case
    Lit (LBool b) -> VBool b

    Lit (LNat n) -> VNat n

    Lit LNil -> VTree VNil
    Lit (LNode l x r) ->
      let VTree l' = rec env l
          VTree r' = rec env r
          x' = rec env x
      in
      VTree (VNode l' x' r')

    Var x ->
      case M.lookup x env of
        Just v -> v
        Nothing -> error $ "`" <> T.unpack x <> "` is not in scope"

    Abs x e ->
      VClosure x e env

    App e1 e2 ->
      let t = rec env e1
          u = rec env e2
      in
      case t of
        VClosure x e s ->
          let env' = global `M.union` M.insert x u s
          in
          rec env' e
        _ -> error "expected LHS to be abstraction"

    Let x e1 e2 ->
      rec env (App (Abs x e2) e1)

    Ite e1 e2 e3 ->
      let VBool p = rec env e1
          u = rec env e2
          v = rec env e3
      in
      if p then u else v

    Cmp op e1 e2 ->
      let t = rec env e1
          u = rec env e2
      in
      if op == CmpEq
        then VBool (t == u)
        else
          let ok = [(CmpLt, LT), (CmpGt, GT)]
              c  = cmp t u
          in
          VBool $ (op, c) `elem` ok

    Match e1 cs ->
      let v = rec env e1
      in
      match v (NE.toList cs)

  where
    rec = eval global

    match :: Value -> [(Pattern, Expr)] -> Value
    match e [] = error $ "match: " <> T.unpack (pretty . toExpr $ e)
    match (VTree VNil) ((PNil, e) : _) = rec env e
    match (VTree (VNode l x r)) ((PNode x1 x2 x3, e) : _) =
      let env' = M.insert x1 (VTree l)
               . M.insert x2 x
               . M.insert x3 (VTree r)
               $ env
      in
      rec env' e
    match l (_ : xs) = match l xs

cmp :: Value -> Value -> Ordering
cmp (VNat n)  (VNat m)  = compare n m
cmp (VBool a) (VBool b) = compare a b
cmp (VTree t) (VTree u) = error "cmp: tree values cannot be compared"
cmp _         _         = error "cmp: values cannot be compared (type mismatch)"
