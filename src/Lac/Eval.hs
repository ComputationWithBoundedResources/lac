module Lac.Eval where

import           Data.Expr.Types

import           Data.Expr.Pretty
import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Debug.Trace

eval :: Map Text Expr -> Expr -> Expr
eval env expr =
  case expr of
    L (LNode e1 e2 e3) -> L $ LNode (eval env e1) (eval env e2) (eval env e3)
    L l -> L l

    Var x ->
      case M.lookup x env of
        Just e -> e
        Nothing -> expr

    App e1 e2 ->
      case eval env e1 of
        Abs x e -> eval env $ subst x e2 e
        _       -> expr

    e1 :<  e2 -> cmp lt e1 e2
    e1 :== e2 -> cmp eq e1 e2
    e1 :>  e2 -> cmp gt e1 e2

    Ite e1 e2 e3 ->
      case eval env e1 of
        L (LBool True)  -> eval env e2
        L (LBool False) -> eval env e3
        _ -> error "e1 evaluates to non-Boolean value"

    Let x e1 e2 -> eval (M.insert x (eval env e1) env) e2

    Match e cs -> match (eval env e) (NE.toList cs)

    Abs _ _ -> expr

    _ -> traceShow expr undefined
  where
    match :: Expr -> [(Pattern, Expr)] -> Expr
    match _ [] = error "match" -- TODO: run-time exception
    match (L LNil) ((PNil, e) : _) = eval env e
    match (L (LNode e1 e2 e3)) ((PNode x1 x2 x3, e) : _) =
      let env' = M.insert x1 (eval env $ e1)
               . M.insert x2 (eval env $ e2)
               . M.insert x3 (eval env $ e3)
               $ env
      in
      eval env' e
    match l (_ : xs) = match l xs

    subst :: Text -> Expr -> Expr -> Expr
    subst x e (Var y) | x == y    = e
                      | otherwise = Var y
    subst x e1 e@(Abs y e2) | x == y    = e
                            | otherwise = Abs y (subst x e1 e2)
    subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)
    subst x e (Let y e1 e2) = Let y (subst x e e1) e2'
      where e2' | x == y    = e2
                | otherwise = subst x e e2
    subst x e (Ite e1 e2 e3) = Ite (subst x e e1) (subst x e e2) (subst x e e3)
    subst x e (Cmp op l r) = Cmp op (subst x e l) (subst x e r)
    subst x e (Match e1 es) = Match (subst x e e1) (fmap (\(a, b) -> (a, subst x e b)) es)
    subst x e lit@(L l) =
      case l of
        LNode e1 e2 e3 -> L $ LNode (subst x e e1) (subst x e e2) (subst x e e3)
        _ -> lit

    lt (LBool False) (LBool True) = True
    lt (LBool _)     (LBool _)    = False
    lt (LNat x) (LNat y) = x < y

    ne x y = lt x y || lt y x

    eq LNil          LNil          = True
    eq LNil          (LNode _ _ _) = False
    eq (LNode _ _ _) LNil          = False
    eq x y = not (ne x y)

    gt x y = lt y x

    cmp op e1 e2 =
      let (L x) = eval env e1
          (L y) = eval env e2
      in
      L $ LBool (op x y)
