{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Expr
import           Data.Term
import           Lac.Eval
import           Lac.Inf

import           Control.Monad          (forM_, void, when)
import           Control.Monad.State    (StateT, get)
import           Control.Monad.Trans    (liftIO)
import           Data.List              (lookup)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Environment.Ext
import qualified System.Repl            as Repl
import           Text.Parsec            (many1, parse)

data ReplState
  = ReplState {
    rsEnv   :: Map Text Expr
  , rsFlags :: [String]
  }
  deriving (Eq, Show)

defaultReplState :: ReplState
defaultReplState = ReplState mempty mempty

main :: IO ()
main = do
  (flags, args) <- partitionArgs <$> getArgs
  forM_ args $ \arg -> do
    r <- parse (many1 decl) arg <$> T.readFile arg
    case r of
      Left e -> print e
      Right decls ->
        do
          let env = M.fromList . map (\(Decl x xs e) -> (x, fromDecl xs e)) $ decls
          let rs = defaultReplState {
                rsEnv = env
              , rsFlags = flags
              }
          repl rs

repl :: ReplState -> IO ()
repl s =
  void $ Repl.repl "> " s $
    \case
      ':' : cmd -> command cmd
      line      -> input line
  where
    command :: String -> StateT ReplState IO Bool
    command "quit" = return False
    command "decls" = rsFlags <$> get >>= go
      where
        go :: [String] -> StateT ReplState IO Bool
        go flags =
          do
            decls <- (map select . M.toList . rsEnv) <$> get
            forM_ decls $ \(Decl n _ e) ->
              liftIO $ do
                T.putStr $ n <> " : "
                showType e
                T.putStr $ n <> " = "
                f e
            when ("--debug" `elem` flags) $
              forM_ decls $ \(Decl _ _ e) ->
                liftIO (debug e)
            return True
          where
            select (name, e) = Decl name [] e

            f | "--ast" `elem` flags = print
              | otherwise            = T.putStrLn . pretty
    command _ = do
      liftIO $ putStrLn "unknown command"
      return True

    input :: String -> StateT ReplState IO Bool
    input line = do
      case parse expr mempty (T.pack line) of
        Left e -> liftIO $ print e
        Right e -> do
          env <- rsEnv <$> get
          flags <- rsFlags <$> get
          liftIO $ do
            when ("--ast" `elem` flags) (print e)
            when ("--debug" `elem` flags) (debug e)
            T.putStrLn . pretty $ eval env e
      return True

    debug e =
      do
        let (eqs, _) = inferType mempty e
        putStrLn "Equations:"
        mapM_ (T.putStrLn . ppEqn . g) eqs
        putStrLn "MGU:"
        case unify eqs of
          Left e -> print e
          Right mgu ->
            mapM_ (T.putStrLn . ppEqn . g) mgu
      where
        g (t, u) = (mapFun T.pack . mapVar (T.pack . show) $ t, mapFun T.pack . mapVar (T.pack . show) $ u)

    showType expr =
      case unify eqs of
        Right mgu
          | Just ty <- lookup (V 0) mgu ->
              let f = mapFun T.pack
                  g = mapVar (\i -> T.pack ("a" <> show i))
              in
              T.putStrLn (ppTerm . f . g $ ty)
          | otherwise ->
              error "this should not happen"
        Left err -> print err
      where
        (eqs, _) = inferType mempty expr
