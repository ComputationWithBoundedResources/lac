{-# LANGUAGE RecordWildCards #-}

module Lac.Analysis.Rules.Share where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      (var')
import           Data.Expr.FromTyped
import           Data.Expr.Typed.Subst
import           Lac.Analysis.Rules.Common

import           Data.List.Ext                  (splits)
import qualified Data.Set                       as S
import qualified Data.Text                      as T

ruleShare :: (Ctx -> Typed -> Gen ProofTree) -> Ctx -> Text -> Typed -> Gen ProofTree
ruleShare rec q z e =
  do
    setRuleName "share"

    let u = Bound 1

    let zs = filter (== z) . var' . fromTyped $ e
    zs' <- replicateM (length zs) (freshVar z)

    let e' = subst z zs' e

    (_, q') <- splitCtx u q [z]
    q'' <- augmentCtx u q' (zip zs' (repeat tyTree))

    qz <- coeff q (IdIdx z)
    q''zs' <- forM zs' $ \z' -> coeff q'' (IdIdx z')
    accumConstr [ CEq (CAtom qz) (CSum (map CAtom q''zs')) ]

    forVec_ q selAll $ \(xs, qx) ->
      case lookup z xs of
        Just v ->
          let xs' = filter (\(x, _) -> x /= z) xs
          in
          forM_ (splits (length zs') v) $ \vs ->
            let i = VecIdx . S.fromList $ xs' ++ zip zs' vs
            in
            do
              c <- coeff q'' i
              accumConstr [ CEq (CAtom qx) (CAtom c) ]
        Nothing ->
          throwError (AssertionFailed "ruleShare")

    r <- prove rec q'' e'

    conclude q e r

freshVar :: Text -> Gen Text
freshVar x = (("$" <>) . (x <>) . T.pack . show) <$> fresh
