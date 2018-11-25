module Data.Term.Term where

data T f v
  = V v
  | F f [T f v]
  deriving (Eq, Show)
