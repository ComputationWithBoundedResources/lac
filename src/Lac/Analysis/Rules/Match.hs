{-# LANGUAGE LambdaCase #-}

module Lac.Analysis.Rules.Match where

import           Lac.Analysis.Rules.Common

import qualified Data.Text.IO              as T

ruleMatch :: Ctx -> Text -> Typed -> (Text, Text, Text) -> Typed -> Gen Ctx
ruleMatch q x e1 (x1, x2, x3) e2 =
  do
    let u = Bound 1
    let m = lengthCtx q - 1

    (_, p) <- splitCtx u q [x]
    liftIO $ do
      putStrLn "P"
      T.putStrLn (ppCtx p)

    (_, r) <- splitCtx u q [x]
    r' <- augmentCtx u r [(x1, tyTree), (x2, tyNat), (x3, tyTree)]
    liftIO $ do
      putStrLn "R"
      T.putStrLn (ppCtx r')

    -- r(vec{a}, a, a, b) = q(vec{a}, a, b)
    let raaabs = vecCoeffsRev r' $
          \case
            (b:a1:a2:as) | a1 == a2, all (== a1) as -> True
            _ -> False
    qaabs <- forM raaabs $ \(VecIdx xs, _) ->
              let a = head xs
                  b = last xs
                  vec = replicate (length xs - 2) a ++ [b]
              in
              coeff q (VecIdx vec)

    forM_ (zip (map snd raaabs) qaabs) $ \(ri, qi) -> tellConstr [CEq (CAtom ri) (CAtom qi)]

    -- p(vec{a}, c) = sum{a+b=c} q(vec{a}, a, b)
    let pacs =
          vecCoeffsRev p $
            \case
              (c:as) | equal as -> True
              _                 -> False
    forM pacs $ \(VecIdx xs, pac) ->
      let a = head xs
          c = last xs
          qabs = vecCoeffsRev q $
            \case
              (b:a:as) | all (== a) as, a + b == c -> True
              _                                    -> False
      in
      tellConstr [ CEq (CAtom pac) (CSum (map (CAtom . snd) qabs)) ]

    -- r_{m+1} = r_{m+2} = q_{m+1}
    i1 <- idx (m + 1) r'
    rm1 <- coeff r' i1
    i2 <- idx (m + 2) r'
    rm2 <- coeff r' i2
    i3 <- idx (m + 1) q
    qm1 <- coeff q i3
    tellConstr
      [ CEq (CAtom rm1) (CAtom rm2)
      , CEq (CAtom rm2) (CAtom qm1)
      ]

    -- r_{(\vec{0}, 1, 0, 0)} = r_{(\vec{0}, 0, 1, 0)} = q_{m+1}

    let r0100s = vecCoeffsRev r' $
          \case
            0:0:1:zs | all (== 0) zs -> True
            _                        -> False
    -- TODO: assert that lists r0100s and r0010s are of length 1
    forM r0100s $ \(VecIdx _, r0100) ->
      let r0010s = vecCoeffsRev r' $
            \case 0:1:0:zs | all (== 0) zs -> True
                  _                        -> False
      in do
      forM r0010s $ \(VecIdx _, r0010) ->
        tellConstr
          [ CEq (CAtom r0100) (CAtom r0010)
          , CEq (CAtom r0010) (CAtom qm1)
          ]

    return q
