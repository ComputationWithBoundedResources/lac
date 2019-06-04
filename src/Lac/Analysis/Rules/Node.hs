{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Rules.Node where

import           Data.Expr.Types
import           Lac.Analysis.Types

import qualified Data.Text.IO as T

genNode :: Ctx -> Literal -> Gen Ctx
genNode ctx (LNode (Var x1) (Var x2) (Var x3)) =
  do
    q1 <- coeffCtx x1 ctx
    q2 <- coeffCtx x3 ctx
    (q100, ctx1) <- insertVecCtx [1, 0, 0] ctx
    (q010, ctx2) <- insertVecCtx [0, 1, 0] ctx1

    ret <- freshCtx
    (qx', ret1) <- insertAstCtx ret

    tellConstr
      [ q1 `CEq` q2
      , q2 `CEq` qx'
      , q100 `CEq` q010
      ]

    return rootCtx

test =
  let node = LNode (Var "x1") (Var "x2") (Var "x3")
      ctx = rootCtx
  in do
  r <- runGen $ do
          ctx1 <- insert "x1" ctx
          ctx2 <- insert "x2" ctx1
          ctx3 <- insert "x3" ctx2

          genNode ctx3 node

  case r of
    (_, output@Output{..}) ->
      mapM_ (\(CEq lhs rhs) -> T.putStrLn $ "(= " <> lhs <> " " <> rhs <> ")") outConstraints
