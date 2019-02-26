{-# LANGUAGE OverloadedStrings #-}

module Lac.TypeInference where

import           Data.Expr.Types
import           Data.Term

import           Control.Monad                  (replicateM)
import           Control.Monad.State.Strict.Ext
import           Data.List                      (find)
import           Data.Text                      (Text)

-- type inference

type Type = T String Int

type Env = [(T String Text, Type)]

tyBool :: Type
tyBool = F "Bool" []

tyNat :: Type
tyNat = F "Nat" []

tyTree :: Type -> Type
tyTree a = F "Tree" [a]

class Typable a where
  infer :: (Env, a, Type) -> State Int [(Type, Type)]

instance Typable Expr where
  infer = inferExprType

inferExprType :: (Env, Expr, Type) -> State Int [(Type, Type)]
inferExprType (env, expr, tau) =
  case expr of
    Lit LNil -> fresh >>= \a -> return [(tau, tyTree (V a))]
    Lit (LNode e1 e2 e3) -> do
      a <- fresh
      xs <- infer (env, e1, tyTree (V a))
      ys <- infer (env, e2, V a)
      zs <- infer (env, e3, tyTree (V a))
      return $ (tau, tyTree (V a)) : concat [xs, ys, zs]
    Lit (LBool True) -> return [(tau, tyBool)]
    Lit (LBool False) -> return [(tau, tyBool)]
    Lit (LNat _) -> return [(tau, tyNat)]
    Var x ->
      let p ((V y), _ ) = x == y
          p _           = False
      in
      case find p env of
        Just (_, ty) ->
          return [(tau, ty)]
        Nothing -> do
          a <- fresh
          return [(tau, V a)]
    App e1 e2 -> do
      a <- fresh
      (++) <$> infer (env, e1, F "->" [V a, tau]) <*> infer (env, e2, V a)
    Abs x e -> do
      a1 <- fresh
      a2 <- fresh
      let env' = (V x, V a1) : env
      (:) <$> pure (tau, F "->" [V a1, V a2]) <*> infer (env', e, V a2)
    Let x e1 e2 -> do
      a <- fresh
      let env' = (V x, V a) : env
      (++) <$> infer (env, e1, V a) <*> infer (env', e2, tau)
    Ite e1 e2 e3 -> do
      xs <- infer (env, e1, tyBool)
      ys <- infer (env, e2, tau)
      zs <- infer (env, e3, tau)
      return $ concat [xs, ys, zs]
    Match e es ->
      do
        a <- fresh
        let ty = tyTree (V a)
        xs <- infer (env, e, ty)
        ys <- concat <$> mapM (f a ty) es
        return $ xs ++ ys
      where
        f a ty (p, e) =
          case p of
            PNil -> infer (env, e, tau)
            PNode l x r ->
              let env' = (V l, ty) : (V x, V a) : (V r, ty) : env
              in
              infer (env', e, tau)
    Cmp _ e1 e2 -> do
      a <- fresh
      xs <- infer (env, e1, V a)
      ys <- infer (env, e2, V a)
      return $ (tau, tyBool) : (xs ++ ys)

instance Typable Program where
  infer = inferProgType

mkProgEnv :: Env -> [Decl] -> State Int [((T String Text, Type), Expr)]
mkProgEnv env decls =
  do
    as <- replicateM (length decls) fresh
    return $ zipWith f decls as
  where
    f decl@(Decl name xs e) a = ((V name, V a), fromDecl xs e)

extractEnv :: Env -> [((T String Text, Type), Expr)] -> Env
extractEnv env decls' = map fst decls' ++ env

inferProgType :: (Env, Program, Type) -> State Int [(Type, Type)]
inferProgType (env, Program decls, tau) =
  do
    decls' <- mkProgEnv env decls
    let env' = extractEnv env decls'
    constraints <- concat <$> mapM (\((_, a), e) -> infer (env', e, a)) decls'
    return constraints

inferType :: Typable a => Env -> a -> ([(Type, Type)], Int)
inferType env expr = runState (infer (env, expr, V 0)) 0
