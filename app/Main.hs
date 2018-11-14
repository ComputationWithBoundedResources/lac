module Main where

import           Data.Expr
import           Lac.Eval

import           Control.Monad          (forM_, when)
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
      Right decls ->
        do
          mapM_ f decls
          when ("--eval" `elem` flags) $
            forM_ decls $ \(Decl _ _ e) ->
                print $ eval mempty e
        where
          f | "--ast" `elem` flags = print
            | otherwise            = T.putStrLn . pretty
