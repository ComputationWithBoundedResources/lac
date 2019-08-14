module Lac.Analysis.Rules.W where

import           Lac.Analysis.Rules.Common

ruleW :: Rule -> Rule
ruleW rec q e =
  do
    setRuleName "w"

    p <- copyCtx q
    p' <- prove rec p e
    q' <- copyCtx p'

    -- Φ(Γ|P) ≤ Φ(Γ|Q)
    forM_ (coeffs p) $ \(i, pc) -> do
      qc <- coeff q i
      accumConstr [ CLe (CAtom pc) (CAtom qc) ]

    -- Φ(Γ|P') ≥ Φ(Γ|Q')
    forM_ (coeffs p') $ \(i, p'c) -> do
      q'c <- coeff q' i
      accumConstr [ CGe (CAtom p'c) (CAtom q'c) ]

    conclude q e q'
