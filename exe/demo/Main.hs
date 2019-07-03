{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Bound
import           Data.Expr
import           Data.Expr.Parser       (expr)
import           Data.Expr.Typed
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Type
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Rules
import           Lac.Analysis.Types

import           Data.List.NonEmpty     (NonEmpty (..))
import           Text.Parsec

main :: IO ()
main = go
  where
    go = getExpr >>= print >> go

getExpr = (\(Right x) -> x) . parse expr "<stdin>" <$> getLine

runExample :: Gen ProofTree -> IO ()
runExample f =
  do
    (result, Output{..}) <- runGen f
    case result of
      Right t ->
        T.writeFile "proof.tex" (latexProofTree t)
      Left e ->
        print e

-- node
example1 =
  let u = Bound 1
  in
  runExample $ do
    ctx <- freshCtx
    ctx' <- augmentCtx u ctx [("l", tyTree), ("x", tyNat), ("r", tyTree)]
    ruleNode ctx' "l" "r"

-- match
example2 =
  let u = Bound 1
  in
  runExample $ do
    ctx <- freshCtx
    ctx' <- augmentCtx u ctx [("t", tyTree), ("e1", tyTree)]
    let e =
          TyMatch
            (TyVar "t", tyTree)
            ((PNil, (TyVar "e1", tyTree))
             :| [(PNode "l" "x" "r", (TyLit (TyLNode (TyVar "l") (TyVar "x") (TyVar "r")), tyTree))])
    dispatch ctx' e

example3 =
  let u = Bound 1
  in
  runExample $ do
    q <- freshCtx
    q' <- augmentCtx u q [("x", tyTree), ("y", tyTree)]
    dispatch q' (TyVar "x")
