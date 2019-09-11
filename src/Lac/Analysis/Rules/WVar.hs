{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.WVar where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import qualified Data.List.Ext             as L
import qualified Data.Vector               as V

ruleWVar
  :: Rule   -- ^ continuation
  -> Ctx    -- ^ context/annotation
  -> Typed  -- ^ expression
  -> [Text] -- ^ variables that are kept
  -> Gen ProofTree
ruleWVar dispatch q e xs =
  do
    setRuleName "w : var"

    let b@(Bound u) = def
    let m = Ctx.length q - 1 -- q = Γ,x:α|Q
    let m' = fromIntegral m

    ((y, ty), r) <- weakenCtx b q xs

    if ty == tyTree
      then do
        -- r_i = q_i
        forM_ (enumRankCoeffs r) $ \(idx, ri) -> do
          qi <- coeff q idx
          accumConstr [CEq (CAtom qi) (CAtom ri)]

        -- r_{(\vec{a},b)} = q_{(\vec{a},0,b)}
        forM_ (L.enum u m') $ \as -> do
          forM_ [0..u] $ \b -> do
            let vab = VecIdx . V.fromList $ as ++ [b]
            let va0b = VecIdx . V.fromList $ as ++ [0, b]
            rab <- coeff r vab
            qa0b <- coeff q va0b
            accumConstr [CEq (CAtom rab) (CAtom qa0b)]

        q' <- prove dispatch r e
        conclude q e q'
      else do
        eqCtx q r
        q' <- prove dispatch r e
        conclude q e q'
