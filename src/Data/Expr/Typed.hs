{-# LANGUAGE GADTs #-}

module Data.Expr.Typed where

import           Data.Expr.Types    (CmpOp (..), Pattern (..))
import           Data.Type

import           Data.List.NonEmpty
import           Data.Text

data TyLiteral
  = TyLNil
  | TyLNode (Typed, Type) (Typed, Type) (Typed, Type) -- we can assume types `Tree Nat`, `Nat`, `Tree Nat` here
  | TyLBool Bool
  | TyLNat Int

data Typed where
  TyLit :: (TyLiteral, Type) -> Typed
  TyVar :: Text -> Typed
  TyCmp :: CmpOp -> (Typed, Type) -> (Typed, Type) -> Typed
  TyIte :: (Typed, Type) -> (Typed, Type) -> (Typed, Type) -> Typed
  TyLet :: Text -> (Typed, Type) -> (Typed, Type) -> Typed
  TyApp :: (Typed, Type) -> (Typed, Type) -> Typed
  TyMatch :: (Typed, Type) -> NonEmpty (Pattern, (Typed, Type)) -> Typed
  TyAbs :: Text -> (Typed, Type) -> Typed

-- TODO: implement `Show` instance through `Expr`
