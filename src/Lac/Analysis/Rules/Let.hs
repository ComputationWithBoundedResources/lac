module Lac.Analysis.Rules.Let where

import           Data.Expr.FromTyped
import           Data.Expr.Types           (var)
import           Lac.Analysis.Rules.Common

import qualified Data.Set                  as S

ruleLet :: Rule -> Ctx -> Typed -> Gen ProofTree
ruleLet dispatch q e@(TyLet x (e1, ty) (e2, _)) =
  do
    setRuleName "let"

    let u = Bound 1

    -- TODO: assert linearity

    let var' = var . fromTyped

    let vq  = ctxVars q   -- all variables in context Q
    let ve1 = var' e1     -- variables in expression e1
    let rem = vq S.\\ ve1 -- remaining variables (i.e. variables in
                          -- expression e2 and variables that later be
                          -- subject to weakening)

    (_, p) <- splitCtx u q (S.toList rem)
    (_, r) <- splitCtx u q (S.toList ve1)
    r' <- augmentCtx u r [(x, ty)]

    s <- prove dispatch p  e1
    t <- prove dispatch r' e2
    -- TODO: cost-free

    --eqCtx s t

    conclude q e s
