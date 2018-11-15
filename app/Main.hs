module Main where

import           Data.Expr
import           Lac.Eval

import           Control.Monad          (forM_, when)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import           System.Environment.Ext
import           System.IO              (getLine)
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
          let env = M.fromList . map (\(Decl x xs e) -> (x, EDecl xs e)) $ decls
          repl env
        where
          f | "--ast" `elem` flags = print
            | otherwise            = T.putStrLn . pretty

repl :: Map Text Binding -> IO ()
repl env = do
  line <- getLine
  when (line /= ":quit") $ do
    case parse expr mempty line of
      Left e -> print e
      Right e -> do
        print e
        print $ eval env e
    repl env
