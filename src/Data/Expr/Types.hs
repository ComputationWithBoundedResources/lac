{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Expr.Types where

import           Data.Text    (Text)

data Expr where
  T :: Expr -> Expr -> Expr -> Expr
  B :: Bool -> Expr
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
  Match :: Expr -> Expr -> (Text, Text, Text, Expr) -> Expr

deriving instance Show Expr
