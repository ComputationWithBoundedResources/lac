module Data.List.Helpers where

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
