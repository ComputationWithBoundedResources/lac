{-# LANGUAGE GADTs #-}

module Data.Expr.Typed where

import           Data.Expr.Types    (Pattern (..))
import           Data.Type

import           Data.List.NonEmpty
import           Data.Text

data Literal
  = TyLNil
  | TyLNode (Typed, Type) (Typed, Type) (Typed, Type)
  | TyLBool Bool
  | TyLNat Int

data Typed where
  TyLit :: (Literal, Type) -> Typed
  TyVar :: Text -> Typed
  TyCmp :: (Typed, Type) -> (Typed, Type) -> Typed
  TyIte :: Typed -> (Typed, Type) -> (Typed, Type) -> Typed
  TyLet :: Text -> (Typed, Type) -> (Typed, Type) -> Typed
  TyApp :: Typed -> (Typed, Type) -> Typed
  TyMatch :: (Typed, Type) -> NonEmpty (Pattern, (Typed, Type)) -> Typed
  TyAbs :: Text -> (Typed, Type) -> Typed

-- TODO: implement `Show` instance through `Expr`
