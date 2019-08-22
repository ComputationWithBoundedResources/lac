module Data.Maybe.Ext (
    module E
  , orElse
  ) where

import           Data.Maybe as E

orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@(Just _) y = x
orElse Nothing    y = y
