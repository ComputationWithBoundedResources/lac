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

    s <- prove dispatch p  e1 -- R'
    t <- prove dispatch r' e2
    -- TODO: cost-free

    -- p_i = q_i
    forM_ (enumRankCoeffs p) $ \(idx, pi) ->
      coeff q idx >>= \qi ->
        accumConstr [ CEq (CAtom pi) (CAtom qi) ]

    -- p'_\ast = r_{k+1}
    -- only possible when x is bound to a value of type tree?
    if ty == tyTree
      then do
        rx <- coeff r' (IdIdx x)
        tx <- coeff t  astIdx
        accumConstr [ CEq (CAtom rx) (CAtom tx) ]
      else
        liftIO $ putStrLn "x is not bound to a value of type tree"

    -- r_j = q_j
    forM_ (enumRankCoeffs r) $ \(idx, rj) ->
      coeff q idx >>= \qj ->
        accumConstr [ CEq (CAtom rj) (CAtom qj) ]

    --eqCtx s t

    q' <- returnCtx u

    conclude q e q'
