{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Pretty (
    Pretty(..)
  ) where

import           Data.Expr.Types

import qualified Data.List.NonEmpty as NE
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T

class Pretty t where
  pretty :: t -> Text

instance Pretty Text where
  pretty = id

instance Pretty Literal where
  pretty (LNat x) = (T.pack . show) x

  pretty (LBool True) = "true"
  pretty (LBool False) = "false"

  pretty LNil = "nil"
  pretty (LNode x y z) = "{" <> T.intercalate ", " (map pretty [x, y, z]) <> "}"

  -- pretty (LNat n) = (T.pack . show) n

instance Pretty Expr where
  pretty (L l) = pretty l
  pretty (V x) = pretty x

  pretty (Ite p e1 e2) = "if " <> pretty p <> " then " <> pretty e1 <> " else " <> pretty e2

  pretty (Fun f as) = pretty f <> " " <> T.intercalate " " (map pretty as)

  pretty (Let x e1 e2) = "let " <> pretty x <> " = " <> pretty e1 <> " in " <> pretty e2

  pretty (Match e cs) =
      "match " <> pretty e <> " with " <> T.intercalate " " cases
    where
      cases =
        map (\(p, e) -> "| " <> pat p <> " -> " <> pretty e) (NE.toList cs)

      pat PNil          = "nil"
      pat (PNode x y z) = "{" <> pretty x <> ", " <> pretty y <> ", " <> pretty z <> "}"

  pretty (l :<  r) = pretty l <> " < " <> pretty r
  pretty (l :== r) = pretty l <> " = " <> pretty r
  pretty (l :>  r) = pretty l <> " > " <> pretty r

instance Pretty Decl where
  pretty (Decl n as e) =
    n <> " " <> T.intercalate " " as <> " = " <> pretty e <> ";"
