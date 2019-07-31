{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Expr.LetNF
import           Data.Expr.Types

import           System.Exit
import           Test.HUnit

main :: IO ()
main =
  do
    Counts _ _ e f <- runTestTT tests
    if e + f > 0
      then exitFailure
      else exitSuccess
  where
    tests = TestList [ TestLabel "testLetNF1" testLetNF1
                     , TestLabel "testLetNF2" testLetNF2
                     ]

iteExpr p = Ite p (Var "e1") (Var "e2")
  where

testLetNF1 =
  let expr = iteExpr (Lit (LBool True))
  in
  TestCase $ assertEqual "let-NF #1" expr (toLetNF expr)

testLetNF2 =
  let cmp = Cmp CmpEq (Var"x") (Var"y")
      expr = iteExpr cmp
  in
  TestCase $ assertBool "let-NF #2" $ expr /= toLetNF expr
