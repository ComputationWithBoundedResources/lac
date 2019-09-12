{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Bound
import           Data.Expr                  hiding (fromDecl)
import           Data.Expr.Typed
import           Data.Term
import           Data.Type
import           Lac
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Rules
import           Lac.Analysis.Types         (Error (..), augmentCtx, emptyCtx,
                                             runGen, throwError)
import           Lac.Eval
import           Lac.TypeInference

import           Control.Monad              (forM_, void, when)
import           Control.Monad.State.Strict (StateT, get)
import           Control.Monad.Trans        (liftIO)
import           Data.Default
import           Data.List                  (isPrefixOf)
import qualified Data.Map.Strict            as M
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.Environment.Ext
import           System.Exit.Ext
import qualified System.Repl                as Repl
import           Text.Parsec                (parse)

main :: IO ()
main = do
  (flags, args) <- partitionArgs <$> getArgs
  case args of
    [] -> putStrLn "please specify at least one input file" >> exitFailure
    a:_ ->
      if "-i" `elem` flags
        then
          interactive flags args
        else
          analyzeProgram a

analyzeProgram :: FilePath -> IO ()
analyzeProgram path = do
  readProg path >>=
    \case
      Left e -> print e
      Right p@Prog{..} -> do
        progValid <- isProgValid p

        when (not progValid) exitFailure

        let decls = inferProgType p
        forM_ decls $ \TypedDecl{..} -> do
          r <- runGen $ do
            (xs, e') <- either (throwError . AssertionFailed) return (unwrap tyDeclType tyDeclExpr)
            let b = def
            q <- emptyCtx b
            q' <- augmentCtx b q xs
            dispatch q' e'
          case r of
            (Left e, _) -> print e
            (Right t, o) -> do
              let texPath = path <> "-" <> T.unpack tyDeclId <> ".tex"
              T.writeFile texPath (latexProofTree t)
              let smtPath = path <> "-" <> T.unpack tyDeclId <> ".smt"
              T.writeFile smtPath $ T.unlines (smtProofTree t)
              putStrLn $ "wrote proof tree to file `" <> texPath <> "`"
              putStrLn $ "wrote SMT constraints to file `" <> smtPath <> "`"

isProgValid :: Prog -> IO Bool
isProgValid Prog{..} =
  do
    forM_ progDecls $ \(Decl f xs e _) -> do
      let global = map fst . M.toList $ progEnv
      let bound = f : xs ++ global

      mapM_
        (\x -> dieT $ "Variable `" <> x <> "` not bound in declaration `" <> f <> "`")
        (unbound' bound e)

      mapM_
        (\x -> dieT $ "Variable `" <> x <> "` shadowed in declaration `" <> f <> "`")
        (shadowed' bound e)

    return True

interactive :: [String] -> [String] -> IO ()
interactive flags args =
  -- TODO: load all declarations, then enter REPL
  forM_ args $ \arg -> do
    r <- readProg arg
    case r of
      Left e -> print e
      Right p ->
        do
          let rs = defaultReplState {
                rsProg = p
              , rsFlags = flags
              }
          repl rs

data ReplState
  = ReplState {
    rsProg  :: Prog
  , rsFlags :: [String]
  }
  deriving (Eq, Show)

defaultReplState :: ReplState
defaultReplState = ReplState mempty mempty

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
          env <- (progEnv . rsProg) <$> get
          flags <- rsFlags <$> get
          liftIO $ do
            when ("--ast" `elem` flags) (print e)
            T.putStrLn . pretty . toExpr $ eval env env e
      return True

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
commands = map (\cmd@ReplCmd{..} -> (replCmdName, cmd)) [cmdQuit, cmdDecls, cmdCheck]

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
            decls <- getTypedProgram
            forM_ decls $ \TypedDecl{..} ->
              liftIO $ T.putStrLn $ tyDeclId <> " : " <> ppTerm' tyDeclType
            return True

-- TODO: better error type
unwrap :: Type -> Typed -> Either Text ([(Text, Type)], Typed)
unwrap = go []
  where
    go :: [(Text, Type)] -> Type -> Typed -> Either Text ([(Text, Type)], Typed)
    go acc (F "->" [τx, τe]) (TyAbs x (e, _)) =
      let acc' = (x, τx) : acc
      in
      go acc' τe e
    go acc (F "->" _) e           = Left "unwrap: expression does not match type (1)"
    go acc _          (TyAbs _ _) = Left "unwrap: expression does not match type (2)"
    go acc _ e =
      Right (reverse acc, e)

cmdCheck :: ReplCmd
cmdCheck = ReplCmd "check" cmd (const "infer constraints for loaded program")
  where
    cmd :: [String] -> StateT ReplState IO Bool
    cmd _ =
      do
        decls <- getTypedProgram
        forM_ decls $ \TypedDecl{..} -> do
          -- TODO: add declarations to context
          liftIO $ do
            ctx' <- runGen $ do
              (xs, e') <- either (throwError . AssertionFailed) return (unwrap tyDeclType tyDeclExpr)
              let b = Bound 1
              q <- emptyCtx b
              q' <- augmentCtx b q xs
              dispatch q' e'
            print ctx'
        return True

getTypedProgram :: StateT ReplState IO [TypedDecl]
getTypedProgram = (inferProgType . rsProg) <$> get
