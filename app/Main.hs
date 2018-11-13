module Main where

import           Data.Expr

import           Control.Monad          (forM_)
import           Data.Text.IO           as T
import           System.Environment.Ext
import           Text.Parsec            (many1, parse)

main :: IO ()
main = do
  (flags, args) <- partitionArgs <$> getArgs
  forM_ args $ \arg -> do
    r <- parse (many1 decl) arg <$> T.readFile arg
    case r of
      Left e -> print e
      Right decls -> mapM_ f decls
        where
          f | "--ast" `elem` flags = print
            | otherwise            = T.putStrLn . pretty
