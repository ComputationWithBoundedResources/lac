module Data.Term.Term where

data T f v
  = V v
  | F f [T f v]
  deriving (Eq, Show)

mapVar :: (v -> w) -> T f v -> T f w
mapVar g (V x)    = V (g x)
mapVar g (F f ts) = F f (map (mapVar g) ts)

mapFun :: (f -> g) -> T f v -> T g v
mapFun f (V x)    = V x
mapFun f (F x xs) = F (f x) (map (mapFun f) xs)
