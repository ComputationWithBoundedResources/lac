module Lac.Analysis.Types.Coeff where

data Coeff
  = Coeff {
    coeffId :: Int
  }
  deriving (Ord, Eq, Show)
