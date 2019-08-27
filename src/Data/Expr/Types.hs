{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Expr.Types where

import           Data.TypeAnn

import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe.Ext     (mapMaybe, orElse)
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)

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
    Lit l        ->
      case l of
        LNode a b c -> sub a ++ sub b ++ sub c
        _           -> []
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
var = S.fromList . var'

var' :: Expr -> [Text]
var' = mapMaybe f . sub
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
  = Decl {
    declId    :: Text
  , declArgs  :: [Text] -- TODO: Use `NonEmpty<Text>` for arguments?
  , declExpr  :: Expr
  , declTySig :: Maybe TypeSig
  }
  deriving (Eq, Show)

-- Convert declaration to expression (abstraction)
--
-- >>> Data.Expr.fromDecl ["x"] (Var "x")
-- Abs "x" (Var "x")
fromDecl :: [Text] -> Expr -> Expr
fromDecl (x:xs) e = Abs x (fromDecl xs e)
fromDecl []     e = e

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

isSimple :: Expr -> Bool
isSimple = (<= 1) . depth

depth :: Expr -> Int
depth (Var _) = 0
depth (Lit l) =
  case l of
    LNil -> 0
    LNode e1 e2 e3 -> 1 + maximum [depth e1, depth e2, depth e3]

    LBool _ -> 0

    LNat _ -> 0
depth (Cmp _ e1 e2) = 1 + maximum [depth e1, depth e2]
depth (Ite x e1 e2) = 2 + maximum [depth x, depth e1, depth e2]
depth (Let _ e1 e2) = 1 + maximum [depth e1, depth e2]
depth (App e1 e2) = 1 + maximum [depth e1, depth e2]
depth (Match t cs) = 2 + depth t + maximum (map (\(_, e) -> depth e) . NE.toList $ cs)
depth (Abs _ e) = 1 + depth e

-- | Find a shadowed variable in expression.
shadowed :: Expr -> Maybe Text
shadowed = shadowed' []

shadowed' :: [Text] -> Expr -> Maybe Text
shadowed' acc e =
  case e of
    Let x e1 e2
      | x `elem` acc -> Just x
      | otherwise    -> let acc' = x:acc
                        in
                        go acc' e1 `orElse` go acc' e2
    Abs x e
      | x `elem` acc -> Just x
      | otherwise    -> let acc' = x:acc
                        in
                        go acc' e
    Match e1 (c1 :| cs) ->
      let cs' = c1:cs
      in
        foldr orElse Nothing $ go acc e1 : map f cs'
      where
        f (PNil, e) = go acc e
        f (PNode x1 x2 x3, e)
          | x1 `elem` acc = Just x1
          | x2 `elem` acc = Just x2
          | x3 `elem` acc = Just x3
          | otherwise =
            let acc' = x1:x2:x3:acc
            in
            go acc' e
    Lit (LNode e1 e2 e3) ->
      go acc e1 `orElse` go acc e2 `orElse` go acc e3
    Lit _ ->
      Nothing
    Var _ ->
      Nothing
    Cmp _ e1 e2 ->
      go acc e1 `orElse` go acc e2
    Ite e1 e2 e3 ->
      go acc e1 `orElse` go acc e2 `orElse` go acc e3
    App e1 e2 ->
      go acc e1 `orElse` go acc e2
  where
    go = shadowed'

-- | Find an unbound variable in expression.
unbound :: Expr -> Maybe Text
unbound = unbound' []

unbound' :: [Text] -> Expr -> Maybe Text
unbound' acc e =
  case e of
    Let x e1 e2 ->
      let acc' = x:acc
      in
      go acc' e1 `orElse` go acc' e2
    Abs x e ->
      let acc' = x:acc
      in
      go acc' e
    Var x
      | x `notElem` acc -> Just x
      | otherwise       -> Nothing
    Match e1 (c1 :| cs) ->
      let cs' = c1:cs
      in
        foldr orElse Nothing $ go acc e1 : map f cs'
      where
        f (PNil, e) = go acc e
        f (PNode x1 x2 x3, e) =
          let acc' = x1:x2:x3:acc
          in
          go acc' e
    Lit (LNode e1 e2 e3) ->
      go acc e1 `orElse` go acc e2 `orElse` go acc e3
    Lit _ ->
      Nothing
    Cmp _ e1 e2 ->
      go acc e1 `orElse` go acc e2
    Ite e1 e2 e3 ->
      go acc e1 `orElse` go acc e2 `orElse` go acc e3
    App e1 e2 ->
      go acc e1 `orElse` go acc e2
  where
    go = unbound'
