{-# LANGUAGE OverloadedStrings #-}

module Data.Term.Pretty (
    ppTerm
  , ppTerm'
  , ppEqn
  , convertTerm
  ) where

import           Data.Term.Term

import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T

ppTerm :: T Text Text -> Text
ppTerm = go False
  where
    go _ (V x) = x
    go l (F "->" [t, u]) = brace l inner
      where inner = go True t <> " -> " <> go False u
    go _ (F f []) = f
    go _ (F f ts) = f <> "(" <> (T.intercalate ", " . map (go False) $ ts) <> ")"

    brace False x = x
    brace True  x = "(" <> x <> ")"

ppTerm' :: T String Int -> Text
ppTerm' = ppTerm . convertTerm

convertTerm :: T String Int -> T Text Text
convertTerm = f . g
  where
    f = mapFun T.pack
    g = mapVar (\i -> T.pack ("a" <> show i))

ppEqn :: (T Text Text, T Text Text) -> Text
ppEqn (t, u) = ppTerm t <> " ~ " <> ppTerm u
