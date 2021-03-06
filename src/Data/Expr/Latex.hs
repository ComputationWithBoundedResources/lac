{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Latex where

import           Data.Expr.Types
import           Lac.PP
import           Latex

import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

instance Latex Expr where
  latex e =
    case e of
      Var "_" -> "\\Box"
      Var x   -> latexVar x
      Match x cs ->
        "\\mathrm{match}\\;" <> latex x <> "\\; \\mathrm{with}\\;" <> cases
        where
          cases = T.intercalate " \\mid " (map texCase (NE.toList cs))
          texCase (p, e) = latex p <> " \\rightarrow " <> latex e
      Ite x e1 e2 ->
        "\\mathrm{if}\\;" <> latex x
          <> "\\; \\mathrm{then}\\;" <> latex e1
          <> "\\; \\mathrm{else}\\;" <> latex e2
      Lit l -> latex l
      Cmp op e1 e2 -> latex e1 <> " " <> op' <> " " <> latex e2
        where
          op' =
            case op of
              CmpEq -> "="
              CmpLt -> "<"
              CmpGt -> ">"
      Let x e1 e2 -> "\\mathrm{let}\\;" <> latexVar x <> " = \\Box\\; \\mathrm{in}\\; \\Box"
      App f x -> latex f <> "\\; \\Box"
      _ -> "\\mathrm{TODO}"

instance Latex Pattern where
  latex PNil          = "\\mathrm{nil}"
  latex (PNode a b c) = "\\langle " <> T.intercalate ", " [a, b, c] <> " \\rangle"

instance Latex Literal where
  latex LNil = "\\mathsf{nil}"
  latex (LNode e1 e2 e3) = "\\langle " <> T.intercalate ", " (map latex [e1, e2, e3]) <> " \\rangle"

  latex (LBool True) = "\\mathtt{true}"
  latex (LBool False) = "\\mathtt{true}"

  latex (LNat n) = T.pack (show n)
