{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Pretty (
    Pretty(..)
  ) where

import           Data.Expr.Types

import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as T (intercalate)

class Pretty t where
  pretty :: t -> Text

instance Pretty Text where
  pretty = id

instance Pretty Expr where
  pretty (T x y z) = "{" <> T.intercalate ", " (map pretty [x, y, z]) <> "}"
  pretty (V x) = pretty x

  pretty (B True) = "true"
  pretty (B False) = "false"

  pretty (Ite p e1 e2) = "if " <> pretty p <> " then " <> pretty e1 <> " else " <> pretty e2

  pretty (Fun f as) = pretty f <> " " <> T.intercalate " " (map pretty as)

  pretty (Let x e1 e2) = "let " <> pretty x <> " = " <> pretty e1 <> " in " <> pretty e2

  pretty (Match e e1 (x1, x2, x3, e2)) = "match " <> pretty e <> " with | nil -> " <> pretty e1 <> " | {" <> T.intercalate ", " (map pretty [x1, x2, x3]) <> "} -> " <> pretty e2

  pretty (l :<  r) = pretty l <> " < " <> pretty r
  pretty (l :== r) = pretty l <> " == " <> pretty r
  pretty (l :>  r) = pretty l <> " > " <> pretty r
