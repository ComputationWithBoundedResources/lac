{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Let where

import           Data.Expr.FromTyped
import           Data.Expr.Types           (var)
import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import qualified Data.List.Ext             as L
import qualified Data.Set                  as S
import qualified Data.Vector               as V
import           Data.Word

import           Debug.Trace

ruleLet
  :: Rule           -- ^ continuation
  -> Ctx            -- ^ context/annotation (i.e. Γ, Δ)
  -> (Typed, Type)  -- ^ expression
  -> Gen ProofTree
ruleLet dispatch q e@(TyLet x (e1, τx) (e2, τe2), τe) =
  do
    setRuleName "let"

    let u@(Bound ub) = def

    -- TODO: check variable ordering

    -- TODO: assert linearity

    let var' = var . fromTyped

    let vΓΔ = ctxVars q   -- all variables in context Q
    let vΓ  = var' e1     -- variables in expression e1
    let vΔ  = vΓΔ S.\\ vΓ -- remaining variables (i.e. variables in
                          -- expression e2 and variables that later will be
                          -- subject to weakening)

    (_, p) <- splitCtx u q (S.toList vΔ)
    (_, r) <- splitCtx u q (S.toList vΓ)
    r' <- augmentCtx u r [(x, τx)]

    let m = Ctx.length p
    let m' = fromIntegral m :: Word8
    let k = Ctx.length r
    let k' = fromIntegral k :: Word8

    s <- prove dispatch p  (e1, τx)  -- R'
    t <- prove dispatch r' (e2, τe2) -- P'

    -- p_i = q_i
    forM_ [1..m'] $ \i -> do
      let idx = RankIdx i
      pi <- coeff p idx
      qi <- coeff q idx
      accumConstr [ CEq (CAtom pi) (CAtom qi) ]

    -- p_{(\vec{a},c)} = q_{(\vec{a},\vec{0},c)}
    let allVecA = L.enum ub m'
    let vecΔ0 = replicate k 0
    forM_ allVecA $ \xs ->
      forM_ [0..ub] $ \c -> do
        pac <- coeff p $ VecIdx . V.fromList $ xs ++ [c]
        qa0c <- coeff q $ VecIdx . V.fromList $ xs ++ vecΔ0 ++ [c]
        accumConstr [ CEq (CAtom pac) (CAtom qa0c) ]

    if τx == tyTree
      then do
        -- p'_\ast = r_{k+1}
        rx <- coeff r' (RankIdx $ k' + 1)
        tx <- coeff t  astIdx
        accumConstr [ CEq (CAtom rx) (CAtom tx) ]

        -- p'_{(a,c)} = r_{(\vec{0},a,c)}
        forM_ [0..ub] $ \a ->
          forM_ [0..ub] $ \c -> do
            p'ac <- coeff t  $ VecIdx . V.fromList $ a :          [c]
            r0ac <- coeff r' $ VecIdx . V.fromList $ a : vecΔ0 ++ [c]
            accumConstr [ CEq (CAtom p'ac) (CAtom r0ac) ]
      else
        liftIO $ putStrLn "x is not bound to a value of type tree"

    case L.enum ub k' of
      [] -> return ()
      (_:vbs) -> do
        forM_ vbs $ \vb -> do
          (_, w) <- splitCtx u q (S.toList vΔ)

          -- p^{\vec{b}}_{(\vec{a},c)} = q_{(\vec{a},\vec{b},c)}
          forM_ allVecA $ \va ->
            forM_ [0..ub] $ \c -> do
              pbac <- coeff w $ VecIdx . V.fromList $ va ++       [c]
              qabc <- coeff q $ VecIdx . V.fromList $ va ++ vb ++ [c]
              accumConstr [ CEq (CAtom pbac) (CAtom qabc) ]

          w' <- prove (costFree dispatch) w (e1, τx)

          -- p'^{\vec{b}}_{(a,c)} = r_{(\vec{b},a,c)}
          forM_ [0..ub] $ \a ->
            forM_ [0..ub] $ \c -> do
              p'bac <- coeff w' $ VecIdx . V.fromList $       [a, c]
              rbac  <- coeff r' $ VecIdx . V.fromList $ vb ++ [a, c]
              accumConstr [ CEq (CAtom p'bac) (CAtom rbac) ]

    -- r_j = q_{m+j}
    forM_ [1..k'] $ \j -> do
      rj <- coeff r' $ RankIdx j
      qmj <- coeff q $ RankIdx (m' + j)
      accumConstr [ CEq (CAtom rj) (CAtom qmj) ]

    -- TODO
    --eqCtx s t

    q' <- returnCtx u

    conclude q e q'

letOrder
  :: Ctx    -- ^ context/annotation
  -> Typed  -- ^ expression e1, i.e. let x = e1 in e2
  -> [Text]
letOrder q e1 = filter (`elem` ts) $ S.toList vΓ ++ S.toList vΔ
  where
    var' = var . fromTyped

    vΓΔ = ctxVars q   -- all variables in context Q
    vΓ  = var' e1     -- variables in expression e1
    vΔ  = vΓΔ S.\\ vΓ -- remaining variables (i.e. variables in
                      -- expression e2 and variables that later will be
                      -- subject to weakening)

    ts = Ctx.trees q
