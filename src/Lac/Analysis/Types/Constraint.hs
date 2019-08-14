{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Types.Constraint (
    Constraint(..)
  , CExpr(..)
  , toSMT
  , constrCoefficients
  ) where

import           Lac.Analysis.Types.Coeff

import           Data.Text                (Text)
import qualified Data.Text                as T

data Constraint
  = CEq CExpr CExpr
  | CLe CExpr CExpr
  | CGe CExpr CExpr
  deriving (Eq, Show)

data CExpr
  = CAtom Coeff
  | CInt Int
  | CSum [CExpr]
  deriving (Eq, Show)

toSMT :: Constraint -> Text
toSMT (CEq e1 e2) = "(= " <> texCExpr e1 <> " " <> texCExpr e2 <> ")"
toSMT (CLe e1 e2) = "(<= " <> texCExpr e1 <> " " <> texCExpr e2 <> ")"
toSMT (CGe e1 e2) = "(>= " <> texCExpr e1 <> " " <> texCExpr e2 <> ")"

texCExpr (CAtom (Coeff i)) = "x" <> T.pack (show i)
texCExpr (CSum es) = "(+ " <> T.intercalate " " (map texCExpr es) <> ")"

constrCoefficients :: Constraint -> [Coeff]
constrCoefficients c =
  let (a, b) = case c of
                CEq a b -> (a, b)
                CLe a b -> (a, b)
                CGe a b -> (a, b)
  in
    f a ++ f b
  where
    f (CAtom c) = [c]
    f (CSum es) = concatMap f es
