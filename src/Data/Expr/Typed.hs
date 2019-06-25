{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Expr.Typed where

import           Data.Expr.Types    (CmpOp (..), Pattern (..))
import           Data.Term.Pretty   (ppTerm')
import           Data.Type

import           Data.List.NonEmpty
import           Data.Text          (Text)
import qualified Data.Text          as T

data TyLiteral
  = TyLNil
  | TyLNode Typed Typed Typed
  | TyLBool Bool
  | TyLNat Int

deriving instance Show TyLiteral

data Typed where
  TyLit :: TyLiteral -> Typed
  TyVar :: Text -> Typed
  TyCmp :: CmpOp -> (Typed, Type) -> (Typed, Type) -> Typed
  TyIte :: (Typed, Type) -> (Typed, Type) -> (Typed, Type) -> Typed
  TyLet :: Text -> (Typed, Type) -> (Typed, Type) -> Typed
  TyApp :: (Typed, Type) -> (Typed, Type) -> Typed
  TyMatch :: (Typed, Type) -> NonEmpty (Pattern, (Typed, Type)) -> Typed
  TyAbs :: (Text, Type) -> (Typed, Type) -> Typed

deriving instance Show Typed

ppTyped :: (Typed, Type) -> Text
ppTyped (expr, ty) = "(" <> go expr <> " : " <> ppTerm' ty <> ")"
  where
    go =
      \case
        TyLit TyLNil -> "nil"
        -- TyLit (TyNode, _) -> ...
        TyLit (TyLBool True) -> "true"
        TyLit (TyLBool False) -> "false"
        TyLit (TyLNat n) -> T.pack . show $ n

        TyVar x -> T.pack . show $ x

        TyApp e1 e2 -> "(" <> ppTyped e1 <> " " <> ppTyped e2 <> ")"
