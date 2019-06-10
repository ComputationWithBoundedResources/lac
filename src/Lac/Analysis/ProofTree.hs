{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.ProofTree where

import           Latex

import           Data.Text (Text)
import qualified Data.Text as T

-- * Proof tree

-- | An intermediate representation of a proof tree that can later be converted to LaTex code.
data ProofTree
  = ProofTree Text [ProofTree]
  deriving (Eq, Show)

instance Latex ProofTree where
  latex (ProofTree concl premises)
    | null premises = concl
    | otherwise     = "\\infer{" <> concl <> "}{" <> T.intercalate " & " (map latex premises) <> "}"

provedBy :: Text -> [ProofTree] -> ProofTree
concl `provedBy` ts = ProofTree concl ts

assume :: Text -> ProofTree
assume s = ProofTree s []
