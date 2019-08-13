module Data.List.Ext (
    module E
  , equal
  , for
  , elemElem
  ) where

import           Data.List as E

equal :: Eq a => [a] -> Bool
equal [] = True
equal [_] = True
equal (x1:x2:xs) =
  if x1 == x2
    then equal (x2:xs)
    else False

for :: [a] -> (a -> b) -> [b]
for = flip map

elemElem :: Eq a => a -> [a] -> Bool
elemElem x ys =
  case break (== x) ys of
    (_, [])      -> False
    (_, (_:ys')) -> x `elem` ys'
