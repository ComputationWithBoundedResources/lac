{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.ProofTree (
    ProofTree(..)
  , latexProofTree
  ) where

import           Data.Expr.FromTyped
import           Data.Expr.Latex               (latexVar)
import           Data.Expr.Typed
import           Lac.Analysis.RuleName
import           Lac.Analysis.Types.Coeff
import           Lac.Analysis.Types.Constraint
import           Lac.Analysis.Types.Ctx
import           Latex

import qualified Data.Map.Strict               as M
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Tuple                    (swap)

data ProofTree
  = ProofTree {
      ptConclusion  :: (Ctx, Typed, Ctx)
    , ptRuleName    :: RuleName
    , ptConstraints :: [Constraint]
    , ptSubtrees    :: [ProofTree]
    }
  deriving (Show)

latexProofTree :: ProofTree -> Text
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

        latexCoeff (Coeff c) =
          case lookup (Coeff c) (f r) of
            Just i -> g r i
            Nothing ->
              case lookup (Coeff c) (f q) of
                Just i -> g q i
                Nothing -> "\\#_{" <> T.pack (show c) <> "}"
          where
            f = map swap . M.toList . ctxCoefficients
            g Ctx{..} idx =
              "q^{" <> T.pack (show ctxId) <> "}"
                <> "_{" <> h idx <> "}"
            h (IdIdx x) = latexVar x
            h (VecIdx a) =
              "(" <> T.intercalate ", " (map (T.pack . show) a) <> ")"
            h AstIdx = "\\ast"
