module Data.Expr.LetNF where

import           Data.Expr.Types

import           Control.Monad.State.Strict.Ext
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Let normal form

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
        App lhs rhs ->
          do
            let es = unfoldl1 unApp e

            rs <- mapM normalize es

            let xs = map fst rs
            let call = foldl1 App (map Var xs)

            return $ mkLet (concatMap snd rs) call
        Var _ ->
          return e
        Cmp cmpOp lhs rhs ->
          do
            (lhs', ls1) <- normalize lhs
            (rhs', ls2) <- normalize rhs
            return $ mkLet (ls1 <> ls2) (Cmp cmpOp (Var lhs') (Var rhs'))
        Let x e1 e2 ->
          Let <$> pure x <*> letNF e1 <*> letNF e2
        Abs x e ->
          Abs <$> pure x <*> letNF e

fresh' :: Monad m => StateT Int m Text
fresh' = decorate <$> fresh

decorate :: Int -> Text
decorate = T.pack . ("$" ++) . show

normalize :: Monad m => Expr -> StateT Int m (Text, [(Text, Expr)])
normalize e =
  case e of
    Var x -> return (x, [])
    _ ->
      do
        x' <- fresh'
        e' <- letNF e
        return (x', [(x', e')])

mkLet :: [(Text, Expr)] -> Expr -> Expr
mkLet ls e = foldr f e ls
  where
    f (x, e1) e2 = Let x e1 e2

unfoldl1 :: (a -> Maybe (a, a)) -> a -> [a]
unfoldl1 f = go []
  where
    go acc x =
      case f x of
        Just (a, b) -> go (b : acc) a
        Nothing     -> x : reverse acc

unApp :: Expr -> Maybe (Expr, Expr)
unApp e =
  case e of
    App e1 e2 -> Just (e1, e2)
    _         -> Nothing

instance LetNF Literal where
  letNF l =
    case l of
      LNode e1 e2 e3 -> LNode <$> letNF e1 <*> letNF e2 <*> letNF e3
      _              -> return l
