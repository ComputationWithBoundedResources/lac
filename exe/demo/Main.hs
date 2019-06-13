{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Bound
import           Data.Expr          (Pattern (..))
import           Data.Expr.Parser   (expr)
import           Data.Expr.Typed
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Term          (T (..))
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Type
import           Lac.Analysis.Rules
import           Lac.Analysis.Types
import           Lac.TypeInference

import           Text.Parsec

main :: IO ()
main = go
  where
    go = getExpr >>= print >> go

getExpr = (\(Right x) -> x) . parse expr "<stdin>" <$> getLine

runExample :: Gen Ctx -> IO ()
runExample f =
  do
    (result, Output{..}) <- runGen f
    case result of
      Right ctx -> do
        putStrLn "--"
        putStrLn "-- FINAL CONTEXT --"
        putStrLn "--"
        T.putStrLn $ ppCtx ctx
        putStrLn "constraints:"
        T.putStrLn $ T.intercalate "\n" (map ppConstr outConstraints)
      Left e ->
        print e

-- node
example1 =
  let u = Bound 1
  in
  runExample $ do
    ctx <- freshCtx
    ctx' <- augmentCtx u ctx [("l", tyTree), ("x", tyNat), ("r", tyTree)]
    liftIO $ T.putStrLn (ppCtx ctx')
    ruleNode ctx' "l" "r"

-- match
example2 =
  let u = Bound 1
  in
  runExample $ do
    ctx <- freshCtx
    ctx' <- augmentCtx u ctx [("t", tyTree)]
    liftIO $ T.putStrLn (ppCtx ctx')
    ruleMatch ctx' "t" (TyVar "e1") ("l", "x", "r") (TyLit (TyLNode (TyVar "l") (TyVar "x") (TyVar "r")))
