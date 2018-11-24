module Lac.Inf where

import           Data.Expr.Types

import           Control.Monad.State
import           Data.List           (find)
import           Data.Monoid
import           Data.Text           (Text)

-- type inference

data T f v
  = V v
  | F f [T f v]
  deriving (Eq, Show)

type Type = T String Int

type Env = [(T String Text, Type)]

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
    L LNil -> fresh >>= \a -> return [(tau, tyTree (V a))]
    L (LNode e1 e2 e3) -> do
      a <- fresh
      xs <- infer (env, e1, tyTree (V a))
      ys <- infer (env, e2, V a)
      zs <- infer (env, e3, tyTree (V a))
      return $ (tau, tyTree (V a)) : concat [xs, ys, zs]
    L (LBool True) -> return [(tau, tyBool)]
    L (LBool False) -> return [(tau, tyBool)]
    L (LNat _) -> return [(tau, tyNat)]
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
    Match e es -> do
      a1 <- fresh
      xs <- infer (env, e, tyTree (V a1))
      ys <- concat <$> mapM (\(_, e) -> infer (env, e, tau)) es
      return $ xs ++ ys
    Cmp _ e1 e2 -> do
      xs <- infer (env, e1, tyNat)
      ys <- infer (env, e2, tyNat)
      return $ (tau, tyBool) : (xs ++ ys)

inferType :: Env -> Expr -> ([(Type, Type)], Int)
inferType env expr = runState (infer (env, expr, V 0)) 0

-- unification

lift2 :: (a -> b) -> (a, a) -> (b, b)
lift2 f (x, y) = (f x, f y)

solve :: (Show f, Eq f, Eq v) => [(T f v, T f v)] -> [(T f v, T f v)] -> Either String [(T f v, T f v)]
solve ((V x, t) : xs) s =
  if V x == t
    then solve xs s
    else elim x t xs s
solve ((t@(F _ _), V x) : xs) s =
  elim x t xs s
solve ((F f ts, F g us) : xs) s
  | f == g    = solve ((zip ts us) ++ xs) s
  | otherwise = Left $ show f <> " /= " <> show g
solve [] s = Right s

elim :: (Show f, Eq f, Eq v) => v -> T f v -> [(T f v, T f v)] -> [(T f v, T f v)] -> Either String [(T f v, T f v)]
elim x t xs s =
  if occurs x t
    then Left "infinite type"
    else
      let xt = lift2 (subst x t)
          xs' = map xt xs
          s' = map xt s
      in
      solve xs' ((V x, t) : s')

occurs :: Eq v => v -> T f v -> Bool
occurs x (V y)    = x == y
occurs x (F _ ts) = any (occurs x) ts

subst :: Eq v => v -> T f v -> T f v -> T f v
subst x t (F f ts) = F f (map (subst x t) ts)
subst x t (V y)
  | x == y    = t
  | otherwise = V y

unify :: (Show f, Eq f, Eq v) => [(T f v, T f v)] -> Either String [(T f v, T f v)]
unify = flip solve mempty
