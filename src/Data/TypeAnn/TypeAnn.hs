module Data.TypeAnn.TypeAnn where

data TypeAnn
  = TypeAnn {
    taRank :: [Int]
  , taVec  :: [([Int], Int)]
  }
  deriving (Eq, Show)
