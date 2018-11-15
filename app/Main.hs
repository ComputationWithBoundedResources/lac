{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Expr
import           Lac.Eval

import           Control.Monad          (forM_, void, when)
import           Control.Monad.State    (StateT, get)
import           Control.Monad.Trans    (liftIO)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Environment.Ext
import           System.IO              (getLine)
import qualified System.Repl            as Repl
import           Text.Parsec            (many1, parse)

data Repl
  = Repl {
    decls :: Map Text Binding
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  (flags, args) <- partitionArgs <$> getArgs
  forM_ args $ \arg -> do
    r <- parse (many1 decl) arg <$> T.readFile arg
    case r of
      Left e -> print e
      Right decls ->
        do
          let env = M.fromList . map (\(Decl x xs e) -> (x, EDecl xs e)) $ decls
          repl env

repl :: Map Text Binding -> IO ()
repl env =
  void $ Repl.repl "> " (Repl env) $
    \case
      ':' : cmd -> command cmd
      line      -> input line
  where
    command :: String -> StateT Repl IO Bool
    command "quit" = return False
    command "decls" =
      do
        (mapMaybe select . M.toList . decls) <$> get >>= mapM_ (liftIO . f)
        return True
      where
        select (name, EDecl xs e) = Just $ Decl name xs e
        select _                  = Nothing

        f | "--ast" `elem` [] = print
          | otherwise         = T.putStrLn . pretty
    command _ = do
      liftIO $ putStrLn "unknown command"
      return True

    input :: String -> StateT Repl IO Bool
    input line = do
      case parse expr mempty (T.pack line) of
        Left e -> liftIO $ print e
        Right e -> liftIO $ do
          -- print e
          T.putStrLn . pretty . toExpr $ eval env e
      return True
