module Latex (
    Latex(..)
  )
  where

import           Data.Text (Text)

class Latex a where
  latex :: a -> Text
