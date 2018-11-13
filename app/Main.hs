module Main where

import           Data.Expr

import           Control.Monad      (forM_)
import           Data.Text.IO       as T
import           System.Environment
import           Text.Parsec        (parse, many1)

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> do
    r <- parse (many1 decl) arg <$> T.readFile arg
    case r of
      Left e -> print e
      Right decls -> mapM_ (T.putStrLn . pretty) decls
