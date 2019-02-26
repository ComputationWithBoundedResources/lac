{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      hiding (expr)
import           Lac.Analysis.Types
import           Lac.TypeInference

import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Text                      (Text)
import qualified Data.Text                      as T

data Ctx
  = Ctx {
    ctxName    :: Text
  , ctxMembers :: [(Text, AnTy)]
  }
  deriving (Eq, Show)

nullCtx :: Text -> Ctx
nullCtx name = Ctx name mempty

data AnTy
  = AnTy {
    anTyType       :: Type
  , anTyAnnotation :: ()
  }
  deriving (Eq, Show)

dispatch :: Ctx -> Expr -> Ctx -> Gen ()
dispatch ctx expr ctx' = do
  gen <- case expr of
    Abs _ _   -> throwError $ NotImplemented "abstraction"
    Lit _     -> throwError $ NotImplemented "literal"
    Cmp _ _ _ -> throwError $ NotImplemented "comparison"
    Ite _ _ _ -> throwError $ NotImplemented "if-then-else"
    Let _ _ _ -> throwError $ NotImplemented "let"
    App _ _   -> throwError $ NotImplemented "application"
    Match _ _ -> return genMatch
    Var _     -> throwError $ NotImplemented "variable"
  gen ctx expr ctx'

type Rule a = Ctx -> Expr -> Ctx -> Gen a

genMatch :: Rule ()
genMatch ctx (Match (Var x) ((PNil, e1) :| [(PNode x1 x2 x3, e2)])) ctx' =
  do
    (ty, ctx') <- splitCtx x ctx
    liftIO $ print ty
    return ()
genMatch _ _ _ =
  throwError $ NotApplicable "match"

splitCtx :: Text -> Ctx -> Gen (AnTy, Ctx)
splitCtx x ctx@Ctx{..} =
  case lookup x ctxMembers of
    Just ty -> let ctx' = ctx { ctxMembers = delete' x ctxMembers }
               in
               return (ty, ctx')
    Nothing -> throwError $ AssertionFailed $ "variable `" <> x <> "` does not appear in context"

delete' :: Eq a => a -> [(a, b)] -> [(a, b)]
delete' _ [] = []
delete' k ((k1, v1) : xs)
  | k == k1   = delete' k xs
  | otherwise = (k1, v1) : delete' k xs

freshCtx :: Gen Ctx
freshCtx = do
  i <- fresh
  let name = T.pack $ "C_{" <> show i <> "}"
  return $ nullCtx name
