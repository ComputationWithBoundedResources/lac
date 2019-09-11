{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Rules.Share where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      (var')
import           Data.Expr.FromTyped
import           Data.Expr.Typed.Rename
import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx         as Ctx

import qualified Data.List.Ext                  as L
import qualified Data.Text                      as T
import qualified Data.Vector                    as V

import           Debug.Trace

ruleShare :: Rule -> Text -> Ctx -> Typed -> Gen ProofTree
ruleShare rec z q@Ctx.Ctx{..} e =
  do
    setRuleName "share"

    let m = Ctx.length q - 1
    let m' = fromIntegral m

    case L.splitAt m . Ctx.trees $ q of
      (_, [z']) | z == z' ->
        return ()
      _ ->
        assert False $ "ruleShare: variable " <> z <> " must appear at end of context"

    let bound@(Bound u) = def

    -- generate fresh variables all occurences of variable z
    let zs = filter (== z) . var' . fromTyped $ e
    let nzs = length zs
    zs' <- replicateM nzs (freshVar z)

    -- rename all occurences of variable z
    let e' = rename z zs' e

    -- generate updated context
    (_, q1) <- splitCtx bound q [z]
    q2 <- augmentCtx bound q1 (zip zs' (repeat tyTree))

    -- q_{m+1} = q^2_{m+1}, ..., q_{m+1} = q^2_{m+n}
    qz <- coeff q (RankIdx (m' + 1))
    q''zs' <- forM [(m + 1) .. (m + nzs)] $ \i -> coeff q2 (RankIdx . fromIntegral $ i)
    accumConstr [ CEq (CAtom qz) (CSum (map CAtom q''zs')) ]

    forM_ (L.enum u m') $ \xs ->
      forM_ [0..u] $ \y ->
        forM_ [0..u] $ \c -> do
          let v1 = VecIdx . V.fromList $ xs ++ [y, c]
          qv <- coeff q v1
          forM_ (L.splits nzs y) $ \ys -> do
            let v2 = VecIdx . V.fromList $ xs ++ ys ++ [c]
            q2zi <- coeff q2 v2
            accumConstr [ CEq (CAtom qv) (CAtom q2zi) ]

    q' <- prove rec q2 e'

    conclude q e q'

freshVar :: Text -> Gen Text
freshVar x = (("$" <>) . (x <>) . T.pack . show) <$> fresh
