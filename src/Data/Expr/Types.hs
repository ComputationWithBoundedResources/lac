{-# LANGUAGE GADTs #-}

module Data.Expr.Types where

import           Data.Text    (Text)
import qualified Data.Text    as T

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
  Match :: Text -> Expr -> (Text, Text, Text, Expr) -> Expr
