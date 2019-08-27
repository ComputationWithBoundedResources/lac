{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Pretty (
    Pretty(..)
  , ppDecl
  ) where

import           Data.Expr.Types
import           Lac.PP.Pretty

import qualified Data.List.NonEmpty        as NE
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Text.PrettyPrint.HughesPJ as PP

instance Pretty Text where
  doc = PP.text . T.unpack

instance Pretty Literal where
  doc (LNat x) = PP.text . show $ x

  doc (LBool True) = PP.text "true"
  doc (LBool False) = PP.text "false"

  doc LNil = PP.text "nil"
  doc (LNode x y z) = PP.sep [
      PP.text "("
    , doc x
    , PP.text ","
    , doc y
    , PP.text ","
    , doc z
    , PP.text ")"
    ]

instance Pretty Expr where
  doc (Lit l) = doc l

  doc (Var x) = doc x

  doc (Ite p e1 e2) =
    PP.hang (PP.sep [PP.text "if", doc p]) 2 $
      PP.vcat
        [ PP.sep [PP.text "then", doc e1]
        , PP.sep [PP.text "else", doc e2]
        ]

  doc (App e1 e2) = PP.sep [PP.text "(", doc e1, doc e2, PP.text ")"]

  doc (Let x e1 e2) =
    PP.hang
      (PP.hsep
        [ PP.text "let"
        , doc x
        , PP.text "="
        , doc e1
        , PP.text "in"
        ])
      2
      (doc e2)

  doc (Match e cs) =
      PP.hang
        (PP.hsep
          [ PP.text "match"
          , doc e
          , PP.text "with"
          ])
        2
        (PP.vcat cases)
    where
      cases =
        map (\(p, e) -> PP.hsep
                          [ PP.text "|"
                          , pat p
                          , PP.text "->"
                          , doc e
                          ]) (NE.toList cs)

      pat PNil          = PP.text "nil"
      pat (PNode x y z) = PP.sep
                            [ PP.text "("
                            , doc x
                            , PP.text ","
                            , doc y
                            , PP.text ","
                            , doc z
                            , PP.text ")"
                            ]

  doc (l :<  r) = PP.sep [PP.text "(", doc l, PP.text " < ", doc r, PP.text ")"]
  doc (l :== r) = PP.sep [PP.text "(", doc l, PP.text " == ", doc r, PP.text ")"]
  doc (l :>  r) = PP.sep [PP.text "(", doc l, PP.text " > ", doc r, PP.text ")"]

  doc (Abs x e) = PP.sep [PP.text "\\ ", doc x, PP.text " -> (", doc e, PP.text ")"]

instance Pretty Decl where
  pretty (Decl n as e _) =
    n <> args <> " = " <> pretty e <> ";"
    where
      args | null as   = ""
           | otherwise = " " <> T.intercalate " " as

ppDecl :: Decl -> Text
ppDecl (Decl x xs e _) = x <> args <> " = " <> pretty e
  where
    args | null xs   = ""
         | otherwise = " " <> T.intercalate " " xs
