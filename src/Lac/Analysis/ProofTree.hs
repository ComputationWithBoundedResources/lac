{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.ProofTree (
    ProofTree(..)
  , latexProofTree
  ) where

import           Data.Expr.FromTyped
import           Data.Expr.Latex
import           Data.Expr.Typed
import           Data.Type
import           Lac.Analysis.RuleName
import           Lac.Analysis.Types.Coeff
import           Lac.Analysis.Types.Constraint
import           Lac.Analysis.Types.Ctx
import           Latex

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

data ProofTree
  = ProofTree {
      ptConclusion  :: (Ctx, Typed, Ctx)
    , ptRuleName    :: RuleName
    , ptConstraints :: [Constraint]
    , ptSubtrees    :: [ProofTree]
    }
  deriving (Show)

latexProofTree (ProofTree (q, e, r) (RuleName n) cs ts) =
    "\\infer[(\\mathsf{" <> n
      <> "})]{" <> latexCtx q <> " \\vdash " <> latexTyped e <> " : " <> latexCtx r <> "}"
      <> "{"
      <> T.intercalate " & " (latexConstraints cs : map latexProofTree ts)
      <> "}"
  where
    latexTyped = latex . fromTyped
    latexConstraints = const $
        "\\begin{array}{l}"
          <> T.intercalate " \\\\\n" (map latexConstraint cs)
          <> "\\end{array}"
      where
        latexConstraint (CEq e1 e2) = latexCExpr e1 <> " = " <> latexCExpr e2

        latexCExpr (CAtom c) = latexCoeff c
        latexCExpr (CSum es) = T.intercalate " + " (map latexCExpr es)

        latexCoeff (Coeff i) = "q_{" <> T.pack (show i) <> "}"
