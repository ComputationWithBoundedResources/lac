{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Constraint (
    Constraint(..)
  , CExpr(..)
  , toSMT
  ) where

import           Lac.Analysis.Coeff

import           Data.Text          (Text)
import qualified Data.Text          as T

data Constraint
  = CEq CExpr CExpr
  deriving (Eq, Show)

data CExpr
  = CAtom Coeff
  | CSum [CExpr]
  deriving (Eq, Show)

toSMT :: Constraint -> Text
toSMT (CEq e1 e2) = "(= " <> go e1 <> " " <> go e2 <> ")"
  where
    go (CAtom (Coeff i)) = "x" <> T.pack (show i)
    go (CSum es) = "(+ " <> T.intercalate " " (map go es) <> ")"
