module Data.Term.Term where

import           Data.Bifoldable
import           Data.Bifunctor

data T f v
  = V v
  | F f [T f v]
  deriving (Eq, Ord, Show)

instance Bifunctor T where
  bimap _ g (V x)    = V $ g x
  bimap f g (F x ts) = F (f x) (map (bimap f g) ts)

mapVar :: (v -> w) -> T f v -> T f w
mapVar = second

mapFun :: (f -> g) -> T f v -> T g v
mapFun = first

instance Bifoldable T where
  bifoldMap _ g (V x)    = g x
  bifoldMap f g (F t ts) = f t <> mconcat (map (bifoldMap f g) ts)

var :: T f v -> [v]
var = bifoldMap (const []) g
  where
    g x = [x]
