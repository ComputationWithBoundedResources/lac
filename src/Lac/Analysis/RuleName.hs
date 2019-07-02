module Lac.Analysis.RuleName where

import           Data.Text (Text)

newtype RuleName
  = RuleName Text
  deriving (Eq, Show, Ord)
