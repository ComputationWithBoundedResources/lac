module Data.Type where

import           Data.Term
import           Data.Text (Text)

type Env = [(T String Text, Type)]

type Type = T String Int

tyBool :: Type
tyBool = F "Bool" []

tyNat :: Type
tyNat = F "Nat" []

tyTree :: Type
tyTree = F "Tree" [tyNat]

tyFun :: Type -> Type -> Type
tyFun a b = F "->" [a, b]
