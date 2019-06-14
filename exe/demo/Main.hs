{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Bound
import           Data.Expr.Parser   (expr)
import           Data.Expr.Typed
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Type
import           Lac.Analysis.Rules
import           Lac.Analysis.Types

import           Text.Parsec

main :: IO ()
main = go
  where
    go = getExpr >>= print >> go

getExpr = (\(Right x) -> x) . parse expr "<stdin>" <$> getLine

runExample :: Show a => Gen a -> IO ()
runExample f =
  do
    (result, Output{..}) <- runGen f
    case result of
      Right retval -> do
        putStrLn "--"
        putStrLn "-- RETURN VALUE --"
        putStrLn "--"
        print retval
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
    ruleMatch dispatch ctx' "t" (TyVar "e1") ("l", "x", "r") (TyLit (TyLNode (TyVar "l") (TyVar "x") (TyVar "r")))

example3 =
  let u = Bound 1
  in
  runExample $ do
    ctx <- freshCtx
    ctx' <- augmentCtx u ctx [("t1", tyTree), ("t2", tyTree)]
    i1 <- idx 1 ctx'
    i2 <- idx 2 ctx'
    liftIO $ do
      print i1
      print i2
    return ctx'
