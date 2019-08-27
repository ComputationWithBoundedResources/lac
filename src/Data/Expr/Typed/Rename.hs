module Data.Expr.Typed.Rename where

import           Data.Expr.Typed

import           Control.Monad.State
import           Data.Text           (Text)

rename' :: Text -> Typed -> State [Text] Typed
rename' x = go
  where
    go e =
      case e of
        TyLit (TyLNode e1 e2 e3) ->
          do
            e1' <- go e1
            e2' <- go e2
            e3' <- go e3
            return $ TyLit (TyLNode e1' e2' e3')
        TyLit _ -> return e
        TyVar y
          | x == y    -> TyVar <$> x'
          | otherwise -> return e
        TyCmp op (e1, τ1) (e2, τ2) ->
          do
            e1' <- go e1
            e2' <- go e2
            return $ TyCmp op (e1', τ1) (e2', τ2)
        TyIte (e1, τ1) (e2, τ2) (e3, τ3) ->
          do
            e1' <- go e1
            e2' <- go e2
            e3' <- go e3
            return $ TyIte (e1', τ1) (e2', τ2) (e3', τ3)
        TyLet y (e1, τ1) (e2, τ2) ->
          do
            e1' <- go e1
            e2' <- go e2
            return $ TyLet y (e1', τ1) (e2', τ2)
        TyApp (e1, τ1) (e2, τ2) ->
          do
            e1' <- go e1
            e2' <- go e2
            return $ TyApp (e1', τ1) (e2', τ2)
        TyMatch (e1, τ1) cs ->
          do
            e1' <- go e1
            cs' <- forM cs $ \(p, (ei, τi)) -> do
                      ei' <- go ei
                      return (p, (ei', τi))
            return $ TyMatch (e1', τ1) cs'
    x' =
      do
        (y:ys) <- get
        put ys
        return y

rename :: Text -> [Text] -> Typed -> Typed
rename x xs e = fst $ runState (rename' x e) xs
