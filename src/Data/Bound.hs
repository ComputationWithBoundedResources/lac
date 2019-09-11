module Data.Bound (
    Bound(..)
  ) where

import           Data.Default
import           Data.Word

newtype Bound
  = Bound {
    unBound :: Word8
  }
  deriving (Eq, Show)

instance Default Bound where
  def = Bound 1
