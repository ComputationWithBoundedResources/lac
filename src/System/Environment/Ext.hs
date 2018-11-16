module System.Environment.Ext (
    partitionArgs
  , splitFlag
  , parseFlag
  , module E
  ) where

import           Control.Arrow
import           Data.List          (find, isPrefixOf, partition)
import           System.Environment as E

partitionArgs :: [String] -> ([String], [String])
partitionArgs = merge . (partition p *** drop 1) . break (== "--")
  where
    merge ((as, fs1), fs2) = (as, fs1 ++ fs2)

    p :: String -> Bool
    p ('-':_) = True
    p _       = False

splitFlag :: String -> [String] -> Maybe (String, String)
splitFlag prefix flags = do
  flag <- find (prefix `isPrefixOf`) flags
  return $ splitAt (length prefix) flag

parseFlag :: String -> [String] -> (String -> a) -> Maybe a
parseFlag prefix flags f = (f . snd) <$> splitFlag prefix flags
