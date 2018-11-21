module Lac.Inf where

import Control.Monad.State

-- type inference

data Expr
  = Let Int Expr Expr
  | Ite Expr Expr Expr
  | Abs Int Expr
  | App Expr Expr
  | Match Expr [Expr]
  | CTrue
  | CFalse
  | CNat Int
  | CTree
  deriving (Eq, Show)

data T f v
  = V v
  | F f [T f v]
  deriving (Eq, Show)

type Type = T String Int

type Env = [(T String Int, Type)]

fresh :: Monad m => StateT Int m Int
fresh = do
  i <- get
  let i' = i + 1
  put i'
  return i'

tyBool :: Type
tyBool = F "Bool" []

tyNat :: Type
tyNat = F "Nat" []

tyTree :: Type -> Type
tyTree a = F "Tree" [a]

infer :: (Env, Expr, Type) -> State Int [(Type, Type)]
infer (env, expr, tau) =
  case expr of
    CTrue -> return [(tau, tyBool)]
    CFalse -> return [(tau, tyBool)]
    CNat _ -> return [(tau, tyNat)]
    CTree -> do
      a <- fresh
      return [(tau, tyTree (V a))]
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
    Match e es -> do
      a1 <- fresh
      xs <- infer (env, e, tyTree (V a1))
      ys <- concat <$> mapM (\e -> infer (env, e, tau)) es
      return $ xs ++ ys
