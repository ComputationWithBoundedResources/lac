module Lac.Analysis.RuleName where

import           Data.Text (Text)

newtype RuleName
  = RuleName {
    unRuleName :: Text
  }
  deriving (Eq, Show, Ord)
