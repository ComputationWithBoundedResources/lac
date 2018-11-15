module Lac.Eval where

import           Data.Expr.Types

import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Text          (Text)

data Binding
  = EDecl [Text] Expr
  | EVal Literal
  deriving (Eq, Show)

eval :: Map Text Binding -> Expr -> Literal
eval env expr =
  case expr of
    L (LNode e1 e2 e3) -> LNode (L $ eval env e1) (L $ eval env e2) (L $ eval env e3)
    L l -> l

    V x ->
      case M.lookup x env of
        Just (EVal l)     -> l
        Just (EDecl [] e) -> eval env e

    Fun f xs ->
      case M.lookup f env of
        Just (EDecl ys e) -> eval env . foldr f e $ zip xs ys
          where
            f (x, y) e = Let y x e

    e1 :<  e2 -> cmp lt e1 e2
    e1 :== e2 -> cmp eq e1 e2
    e1 :>  e2 -> cmp gt e1 e2

    Ite e1 e2 e3 ->
      case eval env e1 of
        LBool True  -> eval env e2
        LBool False -> eval env e3
        _ -> error "e1 evaluates to non-Boolean value"

    Let x e1 e2 -> eval (M.insert x (EVal . eval env $ e1) env) e2

    Match e cs -> match (eval env e) (NE.toList cs)
  where
    match :: Literal -> [(Pattern, Expr)] -> Literal
    match _ [] = undefined -- TODO: run-time exception
    match LNil ((PNil, e) : _) = eval env e
    match (LNode e1 e2 e3) ((PNode x1 x2 x3, e) : _) =
      let env' = M.insert x1 (EVal . eval env $ e1)
               . M.insert x2 (EVal . eval env $ e2)
               . M.insert x3 (EVal . eval env $ e3)
               $ env
      in
      eval env' e
    match l (_ : xs) = match l xs

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
      let x = eval env e1
          y = eval env e2
      in
      LBool (op x y)
