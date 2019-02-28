{-# LANGUAGE FlexibleInstances #-}

module Data.Term.Latex where

import           Data.Term.Term
import qualified Data.Text      as T
import           Latex

-- instance (Pretty f, Pretty v) => Latex (T f v) where ...
instance Latex (T String Int) where
  latex (V x) = T.pack (show x)
  latex (F f ts) = "\\mathsf{" <> f' <> "}" <> args
    where
      f' = T.pack f
      args | null ts   = ""
           | otherwise = "(" <> T.intercalate ", " (map latex ts) <> ")"
