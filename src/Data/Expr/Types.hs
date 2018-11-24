{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Expr.Types where

import           Data.List.NonEmpty
import           Data.Text          (Text)

data Literal
  = LNil
  | LNode Expr Expr Expr
  | LBool Bool -- LTrue/LFalse?
  | LNat Int
  deriving (Eq, Show)

toExpr :: Literal -> Expr
toExpr = L

data CmpOp
  = CmpLt
  | CmpEq
  | CmpGt
  deriving (Show, Eq)

data Expr where
  -- true, false, nil, {x, y, z}
  L :: Literal -> Expr
  Var :: Text -> Expr
  Cmp :: CmpOp -> Expr -> Expr -> Expr
  -- if e then e else e
  Ite :: Expr -> Expr -> Expr -> Expr
  -- let x = e in e
  Let :: Text -> Expr -> Expr -> Expr
  -- f(x1, ... xN)
  App :: Expr -> Expr -> Expr
  -- match x with | nil -> e | <x, x, x> -> e
  Match :: Expr -> NonEmpty (Pattern, Expr) -> Expr
  -- \x -> e
  Abs :: Text -> Expr -> Expr

deriving instance Show Expr
deriving instance Eq Expr

pattern (:<) :: Expr -> Expr -> Expr
pattern (:<)  e1 e2 = Cmp CmpLt e1 e2

pattern (:==) :: Expr -> Expr -> Expr
pattern (:==) e1 e2 = Cmp CmpEq e1 e2

pattern (:>) :: Expr -> Expr -> Expr
pattern (:>)  e1 e2 = Cmp CmpGt e1 e2

data Pattern
  = PNil
  | PNode Text Text Text
  deriving (Eq, Show)

data Decl
  = Decl Text [Text] Expr
  deriving (Eq, Show)
