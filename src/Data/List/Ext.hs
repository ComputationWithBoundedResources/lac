module Data.List.Ext (
    module E
  , delete'
  , elemElem
  , enum
  , equal
  , for
  , split
  , splits
  , subseq
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

delete' :: Eq a => a -> [(a, b)] -> [(a, b)]
delete' _ [] = []
delete' k ((k1, v1) : xs)
  | k == k1   = delete' k xs
  | otherwise = (k1, v1) : delete' k xs

enum :: Int -> Int -> [[Int]]
enum u = go
  where
    go 0 = []
    go 1 = map (\x -> [x]) range
    go n = [i : xs | i <- range, xs <- go (n - 1)]

    range = [0..u]

split :: Integral a => a -> [(a, a)]
split x = [(a, x - a) | a <- [0..x]]

splits :: Integral a => Int -> a -> [[a]]
splits 0 _ = []
splits 1 x = [[x]]
splits 2 x = [[a, b] | (a, b) <- split x]
splits n x = concat $ [map (a:) (splits (n - 1) (x - a)) | a <- [0..x]]

subseq :: (a -> a -> Bool) -> [a] -> [[a]]
subseq p = go []
  where
    go acc    []     = [reverse acc]
    go []     (y:ys) = go [y] ys
    go (a:as) (y:ys)
        | p a y      =                  go (y:a:as) ys
        | otherwise  = reverse (a:as) : go [y]      ys
