module Data.Bound (
    Bound(..)
  ) where

newtype Bound = Bound { unBound :: Int }
  deriving (Eq, Show)
