{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Expr.Types where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (mapMaybe)
import           Data.Monoid        (mconcat, (<>))
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T

data Literal
  = LNil
  | LNode Expr Expr Expr
  | LBool Bool
  | LNat Int
  deriving (Eq, Show)

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

sub :: Expr -> [Expr]
sub e =
  e :
  case e of
    Var x        -> []
    L l          -> []
    Cmp _ e1 e2  -> sub e1 ++ sub e2
    Ite e1 e2 e3 -> sub e1 ++ sub e2
    Let x e1 e2  -> Var x : sub e1 ++ sub e2
    App e1 e2    -> sub e1 ++ sub e2
    Abs x e      -> Var x : sub e
    Match e1 cs  -> sub e1 ++ concatMap (\(p, e) -> g p ++ sub e) cs
      where
        g PNil          = []
        g (PNode x y z) = map Var [x, y, z]

var :: Expr -> Set Text
var = S.fromList . mapMaybe f . sub
  where
    f (Var x) = Just x
    f _       = Nothing

bvar :: Expr -> Set Text
bvar = S.fromList . mapMaybe f . sub
  where
    f (Let x _ _) = Just x
    f (Abs x _)   = Just x
    f _           = Nothing

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

fromDecl :: [Text] -> Expr -> Expr
fromDecl (x:xs) e = Abs x (fromDecl xs e)
fromDecl []     e = e

data Program
  = Program [Decl]
  deriving (Eq, Show)
