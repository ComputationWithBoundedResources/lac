module Lac.Analysis.Rules.Let where

import           Data.Expr.FromTyped
import           Data.Expr.Types           (var)
import           Lac.Analysis.Rules.Common

import qualified Data.Set                  as S

ruleLet :: Rule -> Ctx -> Typed -> Gen ProofTree
ruleLet dispatch q e@(TyLet x (e1, _) (e2, _)) =
  do
    setRuleName "let"

    let u = Bound 1

    -- TODO: assert linearity

    let xs = ctxVars q
    let v1 = var . fromTyped $ e1
    let v2 = var . fromTyped $ e2

    (_, p) <- splitCtx u q (S.toList $ xs `S.intersection` v2)
    (_, r) <- splitCtx u q (S.toList $ xs `S.intersection` v1)

    s <- prove dispatch p e1
    t <- prove dispatch r e2 -- TODO: cost-free

    eqCtx s t

    conclude q e s
