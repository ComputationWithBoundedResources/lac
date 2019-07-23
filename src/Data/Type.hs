{-# LANGUAGE OverloadedStrings #-}

module Data.Type where

import           Data.Expr.Latex  (latexVar)
import           Data.Term
import           Data.Term.Pretty (convertTerm)
import           Data.Text        (Text)
import qualified Data.Text        as T

type Env = [(T String Text, Type)]

type Type = T String Int

latexType :: Type -> Text
latexType = go False . convertTerm
  where
    go _ (V x) = latexVar x
    go l (F "->" [t, u]) = brace l inner
      where inner = go True t <> " \\rightarrow " <> go False u
    go _ (F f []) = "\\mathsf{" <> f <> "}"
    go _ (F f ts) = "\\mathsf{" <> f <> "}" <> "(" <> (T.intercalate ", " . map (go False) $ ts) <> ")"

    brace False x = x
    brace True  x = "(" <> x <> ")"

tyBool :: Type
tyBool = F "Bool" []

tyNat :: Type
tyNat = F "Nat" []

tyTree :: Type
tyTree = F "Tree" [tyNat]

tyFun :: Type -> Type -> Type
tyFun a b = F "->" [a, b]

tyHole :: Type
tyHole = F "_" []
