module Data.TypeAnn.TypeAnn where

import           Data.Text (Text)

data TypeAnn
  = TypeAnn {
    taRank :: [(Text, Int)]
  , taVec  :: [([Int], Int)]
  }
  deriving (Eq, Show)
