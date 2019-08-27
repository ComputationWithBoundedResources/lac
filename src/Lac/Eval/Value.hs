{-# LANGUAGE LambdaCase #-}

module Lac.Eval.Value where

import           Data.Expr.Types as Expr
import           Data.Map        as M
import           Data.Text       (Text)

type Env = Map Text Value

nullEnv :: Env
nullEnv = M.empty

data Value
  = VClosure Text Expr Env -- ^ closure, i.e. an abstraction @\x.e@ with environment
  | VNat Int
  | VBool Bool
  | VTree TreeValue
  deriving (Eq, Show)

data TreeValue
  = VNil
  | VNode TreeValue Value TreeValue
  deriving (Eq, Show)

class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr TreeValue where
  toExpr =
    \case
      VNil        -> Lit LNil
      VNode l x r -> Lit (LNode (toExpr l) (toExpr x) (toExpr r))

instance ToExpr Value where
  toExpr =
    \case
      VNat n         -> Lit (LNat n)
      VBool a        -> Lit (LBool a)
      VTree t        -> toExpr t
      VClosure x e m -> Abs x $ Prelude.foldr f e (M.toList m)
        where
          f (y, v) e2 = Let y (toExpr v) e2
