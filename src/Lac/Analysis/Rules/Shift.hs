{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Rules.Shift (
    ruleShift
  , pushBack
  ) where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import qualified Data.List.Ext             as L
import qualified Data.Map.Strict           as M
import qualified Data.Vector               as V

ruleShift
  :: Rule   -- ^ continuation
  -> [Text] -- ^ variables to shift right in context
  -> Ctx    -- ^ context/annotation
  -> Typed  -- ^ expression
  -> Gen ProofTree
ruleShift dispatch toOrder q@Ctx.Ctx{..} e =
  do
    setRuleName "shift"

    to <- mkToV
    from <- mkFromV

    let coeffs' =
          L.for (coeffs q) $ \(index, coeff) ->
            case index of
              RankIdx i ->
                let i' = succ $ to V.! pred i
                in
                (RankIdx i', coeff)
              VecIdx v ->
                let v' = V.imap (\i _ -> v V.! (from V.! i)) v
                in
                (VecIdx v', coeff)

    let vars = Ctx.nonTrees q ++ zip toOrder (repeat tyTree)

    r <- freshCtx >>=
      \p -> return $
        p {
          Ctx.ctxCoefficients = M.fromList coeffs'
        , Ctx.ctxVariables = vars
        }

    -- TODO: output constraints instead of generating reordered context?

    q' <- prove dispatch r e
    conclude q e q'
  where
    len = length toOrder

    fromOrder = Ctx.trees q

    mkToV :: Gen (V.Vector Int)
    mkToV = V.fromList . cost <$> mkMap fromOrder toOrder

    mkFromV :: Gen (V.Vector Int)
    mkFromV = V.fromList . cost <$> mkMap toOrder fromOrder

    -- keep cost part as last compoenent
    cost = (++ [len])

    mkMap :: [Text] -> [Text] -> Gen [Int]
    mkMap xs ys =
      forM xs $ \x ->
        case L.elemIndex x ys of
          Just i -> return i
          Nothing ->
            throwError $ AssertionFailed $ "ruleShift: variable " <> x <> " not found"

pushBack :: Ctx -> [Text] -> [Text]
pushBack q@Ctx.Ctx{..} xs = init_ ++ tail_
  where
    (tail_, init_) = L.partition (`elem` xs) . Ctx.trees $ q
