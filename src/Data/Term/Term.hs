module Data.Term.Term where

import Data.Bifunctor

data T f v
  = V v
  | F f [T f v]
  deriving (Eq, Show)

instance Bifunctor T where
  bimap _ g (V x)    = V $ g x
  bimap f g (F x ts) = F (f x) (map (bimap f g) ts)

mapVar :: (v -> w) -> T f v -> T f w
mapVar = second

mapFun :: (f -> g) -> T f v -> T g v
mapFun = first
