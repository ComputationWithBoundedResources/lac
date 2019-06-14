module Data.List.Ext (
    module E
  , equal
  ) where

import           Data.List as E

equal :: Eq a => [a] -> Bool
equal [] = True
equal [_] = True
equal (x1:x2:xs) =
  if x1 == x2
    then equal (x2:xs)
    else False
