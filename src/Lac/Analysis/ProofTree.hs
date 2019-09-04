{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.ProofTree (
    ProofTree(..)
  , latexProofTree
  , smtProofTree
  ) where

import           Data.Expr.FromTyped
import           Data.Expr.Latex               ()
import           Data.Expr.Typed
import           Lac.Analysis.RuleName
import           Lac.Analysis.Types.Coeff
import           Lac.Analysis.Types.Constraint
import           Lac.Analysis.Types.Ctx
import           Lac.PP                        (latexVar, tshow)
import           Latex

import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Tuple                    (swap)
import qualified Data.Vector                   as V

data ProofTree
  = ProofTree {
      ptConclusion  :: (Ctx, Typed, Ctx)
    , ptRuleName    :: RuleName
    , ptConstraints :: [Constraint]
    , ptSubtrees    :: [ProofTree]
    }
  deriving (Show)

-- TODO: show return type
latexProofTree :: ProofTree -> Text
latexProofTree (ProofTree (q, e, r) (RuleName n) cs ts) =
    "\\infer[(\\mathsf{" <> n
      <> "})]{" <> latexCtx q <> " \\vdash " <> latexTyped e <> " : " <> latexRetCtx r <> "}"
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
        latexConstraint (CLe e1 e2) = latexCExpr e1 <> " \\le " <> latexCExpr e2
        latexConstraint (CGe e1 e2) = latexCExpr e1 <> " \\ge " <> latexCExpr e2

        latexCExpr (CAtom c) = latexCoeff c
        latexCExpr (CInt i)  = tshow i
        latexCExpr (CSum es) = T.intercalate " + " (map latexCExpr es)

        latexCoeff (Coeff c) = go contexts
          where
            contexts = [q, r] ++ lookahead ts

            go [] = "\\#_{" <> tshow c <> "}"
            go (q:qs) =
              case lookup (Coeff c) (f q) of
                Just i -> g q i
                Nothing -> go qs

            f = map swap . M.toList . ctxCoefficients
            g q@Ctx{..} idx =
              "q^{" <> tshow ctxId <> "}"
                <> "_{" <> h q idx <> "}"
            h Ctx{..} (RankIdx i)
              | Just _ <- lookup costId ctxVariables = "\\ast"
              | otherwise = tshow i
            h _ (VecIdx v) =
              "(" <> T.intercalate ", " (map tshow . V.toList $ v) <> ")"

lookahead :: [ProofTree] -> [Ctx]
lookahead = concatMap f
  where
    f ProofTree{..} =
      let (q, _, r) = ptConclusion
      in
      [q, r]

-- TODO: recurse
smtProofTree :: ProofTree -> [Text]
smtProofTree ProofTree{..} =
    let constrained = S.fromList . concatMap constrCoefficients $ cs
        allCoeffs = S.fromList $ ptCoefficients q ++ ptCoefficients q'
        unconstrained = allCoeffs S.\\ constrained
        zeroes = map setZero . S.toList $ unconstrained
        assertions = map toSMT cs ++ zeroes
    in
      concat
        [ [ "(set-logic QF_LIA)" ]
        , map declare (S.toList allCoeffs)
        , map (\x -> "(assert " <> x <> ")") assertions
        , [ "(check-sat)"
          , "(get-model)"
          ]
        ]
  where
    (q, _, q') = ptConclusion
    cs = ptConstraints

    setZero (Coeff x) = "(= x" <> tshow x <> " 0)"

    declare (Coeff x) = "(declare-fun x" <> tshow x <> " () Int)"
