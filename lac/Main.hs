{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Expr              hiding (fromDecl)
import           Data.Term
import           Lac.Eval
import           Lac.Eval.Value
import           Lac.TypeInference

import           Control.Monad          (forM_, void, when)
import           Control.Monad.State    (StateT, evalState, get)
import           Control.Monad.Trans    (liftIO)
import           Data.List              (isPrefixOf, lookup)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Environment.Ext
import qualified System.Repl            as Repl
import           Text.Parsec            (parse)

data ReplState
  = ReplState {
    rsEnv   :: Map Text Value
  , rsFlags :: [String]
  }
  deriving (Eq, Show)

defaultReplState :: ReplState
defaultReplState = ReplState mempty mempty

main :: IO ()
main = do
  (flags, args) <- partitionArgs <$> getArgs
  forM_ args $ \arg -> do
    r <- parse prog arg <$> T.readFile arg
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
    command i | i `isPrefixOf` "help" = do
      forM_ commands $ \(n, ReplCmd{..}) ->
        liftIO $
          T.putStrLn $ ":" <> T.pack n <> " - " <> replCmdDesc (T.pack n)
      return True
    command i =
      case match commands i of
        Right ReplCmd{..} -> replCmdFunc mempty
        Left e            -> liftIO (print e) >> return True

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
            T.putStrLn . pretty . toExpr $ eval env env e
      return True

debug :: Typable a => a -> IO ()
debug e =
  do
    let (eqs, _) = inferType mempty e
    putStrLn "Equations:"
    mapM_ (T.putStrLn . ppEqn . g) eqs
    putStrLn "MGU:"
    case unify eqs of
      Left err  -> print err
      Right mgu -> mapM_ (T.putStrLn . ppEqn . g) mgu
  where
    g (t, u) = (f t, f u)
      where
        f = mapFun T.pack . mapVar (T.pack . show)

data ReplErr
  = ReplErr Text
  deriving (Eq, Show)

data ReplCmd
  = ReplCmd {
    replCmdName :: String
  , replCmdFunc :: [String] -> StateT ReplState IO Bool
  , replCmdDesc :: Text -> Text
  }

match :: [(String, ReplCmd)] -> String -> Either ReplErr ReplCmd
match cs i =
  case filter (\(n, _) -> i `isPrefixOf` n) cs of
    [(_,c)]  -> Right c
    []       -> Left (ReplErr "no match")
    xs@(_:_) -> Left (ReplErr $ "ambiguous match: " <> T.intercalate ", " (map (T.pack . fst) xs))

commands :: [(String, ReplCmd)]
commands = map (\cmd@ReplCmd{..} -> (replCmdName, cmd)) [cmdQuit, cmdDecls]

cmdQuit :: ReplCmd
cmdQuit = ReplCmd "quit" (const $ return False) (const "quit program")

cmdDecls :: ReplCmd
cmdDecls = ReplCmd "decls" cmd (const "show loaded declarations")
  where
    cmd _ = rsFlags <$> get >>= go
      where
        go :: [String] -> StateT ReplState IO Bool
        go flags =
          do
            decls <- (map select . M.toList . rsEnv) <$> get

            let program = Program decls
            let env = mempty
            let decls' = evalState (mkProgEnv env decls) 0
            let env' = extractEnv env decls'
            let (eqs, _) = inferType env program
            let maybeMGU = unify eqs

            forM_ decls $ \(Decl n _ e) ->
              liftIO $ do
                T.putStr $ n <> " : "
                case maybeMGU of
                  Right mgu -> do
                    case lookup (V n) env' >>= (\a -> lookup a mgu) of
                      Just ty -> T.putStrLn (ppTerm' ty)
                      _       -> return ()
                  _ -> return ()
                T.putStr $ n <> " = "
                f e
            when ("--debug" `elem` flags) $
              forM_ decls $ \(Decl _ _ e) ->
                liftIO (debug e)
            return True
          where
            select (name, e) = Decl name [] (toExpr e)

            f | "--ast" `elem` flags = print
              | otherwise            = T.putStrLn . pretty
