module Data.Map.Strict.Ext (
    module E
  , deleteAll
  ) where

import           Data.Map.Strict as E

deleteAll :: Ord k => [k] -> Map k v -> Map k v
deleteAll [] m = m
deleteAll (x:xs) m =
  let m' = E.delete x m
  in
  deleteAll xs m'
