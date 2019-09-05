{-# LANGUAGE RecordWildCards #-}

module Lac.Analysis.Rules.Swap where

import           Lac.Analysis.Rules.Common
import qualified Lac.Analysis.Types.Ctx    as Ctx

import qualified Data.List.Ext             as L
import qualified Data.Map.Strict           as M
import qualified Data.Vector               as V

ruleSwap :: Rule -> [Text] -> Ctx -> Typed -> Gen ProofTree
ruleSwap dispatch toOrder q@Ctx.Ctx{..} e =
  do
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
            throwError $ AssertionFailed $ "ruleSwap: variable " <> x <> " not found"

pushBack :: Ctx -> [Text] -> [Text]
pushBack q@Ctx.Ctx{..} xs = map fst $ init_ ++ tail_
  where
    (init_, tail_) = L.partition p (Ctx.ctxVariables q)

    p (x, τ) | τ /= tyTree    = True
             | x `notElem` xs = True
             | otherwise      = False
