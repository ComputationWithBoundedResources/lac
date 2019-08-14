module Data.Bound (
    Bound(..)
  ) where

import           Data.Default

newtype Bound = Bound { unBound :: Int }
  deriving (Eq, Show)

instance Default Bound where
  def = Bound 2
