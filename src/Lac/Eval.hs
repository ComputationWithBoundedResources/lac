module Lac.Eval where

import           Data.Expr

import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Text          (Text)

data Tree a
  = TNode (Tree a) a (Tree a)
  | TNil
  deriving (Eq, Show)

eval :: Map Text Literal -> Expr -> Literal
eval env expr =
  case expr of
    L l -> l
    V x -> let Just l = M.lookup x env in l

    e1 :<  e2 -> undefined
    e1 :== e2 -> undefined
    e1 :>  e2 -> undefined

    Ite e1 e2 e3 ->
      case eval env e1 of
        LBool True  -> eval env e2
        LBool False -> eval env e3
        _ -> error "e1 evaluates to non-Boolean value"

    Let _ _ _ -> undefined

    Fun _ _ -> undefined

    Match e cs -> match (eval env e) (NE.toList cs)
  where
    match :: Literal -> [(Pattern, Expr)] -> Literal
    match _ [] = undefined -- TODO: run-time exception
    match LNil ((PNil, e) : xs) = eval env e
    match (LNode e1 e2 e3) ((PNode x1 x2 x3, e) : xs) =
      -- TODO: bind x1, x2, x3 to environment
      undefined
    match l (_ : xs) = match l xs
