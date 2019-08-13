module Data.List.Helpers (
    delete'
  , enum
  , split
  , splits
  ) where

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
