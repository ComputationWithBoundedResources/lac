module Data.Term.Unify where

import           Data.Term.Term

import           Data.Monoid    ((<>))

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
