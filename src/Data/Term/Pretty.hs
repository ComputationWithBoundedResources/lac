{-# LANGUAGE OverloadedStrings #-}

module Data.Term.Pretty (
    ppTerm
  , ppEqn
  ) where

import           Data.Term.Term

import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T

ppTerm :: T Text Text -> Text
ppTerm (V x)    = x
ppTerm (F "->" [t, u]) = ppTerm t <> " -> " <> ppTerm u
ppTerm (F f ts) = f <> "(" <> (T.intercalate ", " . map ppTerm $ ts) <> ")"

ppEqn :: (T Text Text, T Text Text) -> Text
ppEqn (t, u) = ppTerm t <> " ~ " <> ppTerm u
