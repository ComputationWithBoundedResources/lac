{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Expr.Types where

import           Data.List.NonEmpty
import           Data.Text          (Text)

data Literal
  = LNil
  | LNode Expr Expr Expr
  | LBool Bool -- LTrue/LFalse?
  -- | LNat Int
  deriving (Eq, Show)

data Expr where
  -- true, false, nil, {x, y, z}
  L :: Literal -> Expr
  V :: Text -> Expr
  (:<) :: Expr -> Expr -> Expr
  (:==) :: Expr -> Expr -> Expr
  (:>) :: Expr -> Expr -> Expr
  -- if e then e else e
  Ite :: Expr -> Expr -> Expr -> Expr
  -- let x = e in e
  Let :: Text -> Expr -> Expr -> Expr
  -- f(x1, ... xN)
  Fun :: Text -> [Text] -> Expr
  -- match x with | nil -> e | <x, x, x> -> e
  Match :: Expr -> NonEmpty (Pattern, Expr) -> Expr

deriving instance Show Expr
deriving instance Eq Expr

data Pattern
  = PNil
  | PNode Text Text Text
  deriving (Eq, Show)

data Decl
  = Decl Text [Text] Expr
  deriving (Eq, Show)
