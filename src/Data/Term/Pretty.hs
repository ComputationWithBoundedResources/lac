{-# LANGUAGE OverloadedStrings #-}

module Data.Term.Pretty (
    ppTerm
  , ppTerm'
  , ppEqn
  ) where

import           Data.Term.Term

import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T

ppTerm :: T Text Text -> Text
ppTerm (V x)    = x
ppTerm (F "->" [t, u]) = "(" <> ppTerm t <> " -> " <> ppTerm u <> ")"
ppTerm (F f ts) = f <> "(" <> (T.intercalate ", " . map ppTerm $ ts) <> ")"

ppTerm' :: T String Int -> Text
ppTerm' = ppTerm . convertTerm

convertTerm :: T String Int -> T Text Text
convertTerm = f . g
  where
    f = mapFun T.pack
    g = mapVar (\i -> T.pack ("a" <> show i))

ppEqn :: (T Text Text, T Text Text) -> Text
ppEqn (t, u) = ppTerm t <> " ~ " <> ppTerm u
