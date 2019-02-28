module Data.Expr.Latex where

import           Data.Expr.Types
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import           Latex

instance Latex Expr where
  latex e =
    case e of
      Var x -> x
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
