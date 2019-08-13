{-# LANGUAGE LambdaCase #-}

module Data.Expr.FromTyped where

import           Data.Expr.Typed
import           Data.Expr.Types
import           Data.Type

import           Data.List.NonEmpty as NE

fromTyped :: Typed -> Expr
fromTyped (TyLit l) =
  Lit $
    case l of
      TyLBool b        -> LBool b
      TyLNat n         -> LNat n
      TyLNil           -> LNil
      TyLNode t1 t2 t3 -> LNode (fromTyped t1) (fromTyped t2) (fromTyped t3)
fromTyped (TyVar x) = Var x
fromTyped (TyCmp op (e1, _) (e2, _)) = Cmp op (fromTyped e1) (fromTyped e2)
fromTyped (TyIte (e1, _) (e2, _) (e3, _)) = Ite (fromTyped e1) (fromTyped e2) (fromTyped e3)
fromTyped (TyLet x (e1, _) (e2, _)) = Let x (fromTyped e1) (fromTyped e2)
fromTyped (TyApp (e1, _) (e2, _)) = App (fromTyped e1) (fromTyped e2)
fromTyped (TyAbs (x, _) (e, _)) = Abs x (fromTyped e)
fromTyped (TyMatch (e1, _) cs) =
  let cs' = NE.map (\(p, (e, _)) -> (p, fromTyped e)) cs
  in
  Match (fromTyped e1) cs'

untyped :: Expr -> Typed
untyped =
  \case
    -- TODO: Lit
    Var x -> TyVar x
    Cmp op e1 e2 -> TyCmp op (rec e1) (rec e2)
    Ite e1 e2 e3 -> TyIte (rec e1) (rec e2) (rec e3)
    Let x e1 e2 -> TyLet x (rec e1) (rec e2)
    App e1 e2 -> TyApp (rec e1) (rec e2)
    -- TODO: Match, Abs
  where
    rec e = (untyped e, tyHole)
