module Data.TypeAnn.TypeAnn where

import           Data.Text (Text)

data TypeAnn
  = TypeAnn {
    taRank :: [(Text, Int)]
  , taVec  :: [([Int], Int)]
  }
  deriving (Eq, Show)

instance Semigroup TypeAnn where
  a <> b =
    TypeAnn {
        taRank = taRank a <> taRank b
      , taVec = taVec a <> taVec b
      }

instance Monoid TypeAnn where
  mempty = TypeAnn mempty mempty
