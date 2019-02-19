{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Expr.Types where

import           Control.Monad.State.Ext

import           Data.List.NonEmpty      (NonEmpty)
import           Data.Maybe              (mapMaybe)
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as T

data Literal
  = LNil                 -- ^ @nil@
  | LNode Expr Expr Expr -- ^ @{x, y, z}@
  | LBool Bool           -- ^ @true@, @false@
  | LNat Int             -- ^ @0@, @1@, @42@, ...
  deriving (Eq, Show)

data CmpOp
  = CmpLt -- ^ @<@
  | CmpEq -- ^ @==@
  | CmpGt -- ^ @>@
  deriving (Show, Eq)

data Expr where
  -- | literal, see 'Literal'
  Lit :: Literal -> Expr
  -- | @x@, @map@, @zipWith@, @x'@
  Var :: Text -> Expr
  -- | comparison operator, see 'CmpOp'
  Cmp :: CmpOp -> Expr -> Expr -> Expr
  -- | @if e then e else e@
  Ite :: Expr -> Expr -> Expr -> Expr
  -- | @let x = e in e@
  Let :: Text -> Expr -> Expr -> Expr
  -- | @f(x1, ... xN)@
  App :: Expr -> Expr -> Expr
  -- |
  -- @
  -- match x with
  --   | nil -> e
  --   | {x, x, x} -> e
  -- @
  Match :: Expr -> NonEmpty (Pattern, Expr) -> Expr
  -- | @\\ x -> e@
  Abs :: Text -> Expr -> Expr

-- | List sub-expressions
--
-- >>> -- if true then 1 else 2
-- >>> map pretty . sub $ Ite (Lit (LBool True)) (Lit (LNat 1)) (Lit (LNat 2))
-- ["if true then 1 else 2","true","1","2"]
--
-- Note: The original expression is included in the result.
sub :: Expr -> [Expr]
sub e =
  e :
  case e of
    Var x        -> []
    Lit l        -> []
    Cmp _ e1 e2  -> sub e1 ++ sub e2
    Ite e1 e2 e3 -> sub e1 ++ sub e2 ++ sub e3
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

-- | Function declaration
--
-- The identity function may be represented as follows:
--
-- @
-- Decl "id" ["x"] (Var "x")
-- @
data Decl
  = Decl Text [Text] Expr
  deriving (Eq, Show)

-- Convert declaration to expression (abstraction)
--
-- >>> Data.Expr.fromDecl ["x"] (Var "x")
-- Abs "x" (Var "x")
fromDecl :: [Text] -> Expr -> Expr
fromDecl (x:xs) e = Abs x (fromDecl xs e)
fromDecl []     e = e

data Program
  = Program [Decl]
  deriving (Eq, Show)

isLit :: Expr -> Bool
isLit (Lit _) = True
isLit _       = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _       = False

isTree :: Expr -> Bool
isTree (Lit LNil)          = True
isTree (Lit (LNode _ _ _)) = True
isTree _                   = False

-- TODO: add ghost type NonLetNF and LetNF?

class LetNF a where
  letNF :: Monad m => a -> StateT Int m a

toLetNF :: LetNF a => a -> a
toLetNF = fst . flip runState 0 . letNF

instance LetNF Expr where
  letNF e =
      case e of
        Ite p e1 e2 -> do
          e1' <- letNF e1
          e2' <- letNF e2
          if isVar p
            then return $ Ite p e1' e2'
            else do
              x <- fresh'
              p' <- letNF p
              return $ Let x p' (Ite (Var x) e1' e2')
        Match x cs ->
          do
            cs' <- mapM g cs
            if isVar x
              then return $ Match x cs'
              else do
                y <- fresh'
                x' <- letNF x
                return $ Let y x' (Match (Var y) cs')
          where
            g (lhs, rhs) = letNF rhs >>= \rhs' -> return (lhs, rhs')
        Lit l ->
          Lit <$> letNF l
        -- TODO: rewrite application to normal form
        _ -> return e
      where
        fresh' = decorate <$> fresh

        decorate = T.pack . ("$" ++) . show

instance LetNF Literal where
  letNF l =
    case l of
      LNode e1 e2 e3 -> LNode <$> letNF e1 <*> letNF e2 <*> letNF e3
      _              -> return l
