module Main where

import           Data.Expr

import           Control.Monad      (forM_)
import           Data.Text.IO       as T
import           System.Environment
import           Text.Parsec        (parse)

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> do
    r <- parse expr arg <$> T.readFile arg
    case r of
      Left e -> print e
      Right t -> T.putStrLn (pretty t)
