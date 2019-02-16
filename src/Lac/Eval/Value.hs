module Lac.Eval.Value where

import           Data.Expr.Types as Expr
import           Data.Map        as M
import           Data.Text       (Text)

type Env = Map Text Value

nullEnv :: Env
nullEnv = M.empty

data Value
  = VClosure Text Expr Env
  | VNat Int
  | VBool Bool
  | VTree TreeValue
  deriving (Eq, Show)

data TreeValue
  = VNil
  | VNode TreeValue Value TreeValue
  deriving (Eq, Show)
