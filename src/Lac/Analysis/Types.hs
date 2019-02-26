{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx(..)
  , nullCtx
  , augmentCtx
  , splitCtx
  , AnTy(..)

  , Error(..)
  , Constraint(..)
  , Gen (..)
  , runGen
  , tell
  , throwError
  )
  where

import           Control.Monad.State.Strict.Ext
import           Data.List.Helpers
import           Lac.TypeInference

import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Basic types

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

splitCtx :: Text -> Ctx -> Gen (AnTy, Ctx)
splitCtx x ctx@Ctx{..} =
  case lookup x ctxMembers of
    Just ty -> let ctx' = ctx { ctxMembers = delete' x ctxMembers }
               in
               return (ty, ctx')
    Nothing -> throwError $ AssertionFailed $ "variable `" <> x <> "` does not appear in context"

augmentCtx :: [(Text, AnTy)] -> Ctx -> Gen Ctx
augmentCtx xs ctx@Ctx{..} =
  do
    name <- freshCtxName
    return $
      ctx {
        ctxName    = name
      , ctxMembers = xs ++ ctxMembers
      }

freshCtxName :: Gen Text
freshCtxName = do
  i <- fresh
  return (T.pack $ "C_{" <> show i <> "}")

-- * Execution

data Error
  = NotImplemented Text
  | NotApplicable Text
  | AssertionFailed Text
  deriving (Eq, Show)

data Constraint = Constraint
  deriving (Eq, Show)

newtype Gen a = Gen {
    unGen :: ExceptT Error (StateT Int (WriterT [Constraint] IO)) a
  }
  deriving (
    Functor
  , Applicative
  , Monad
  , MonadError Error
  , MonadState Int
  , MonadWriter [Constraint]
  , MonadIO
  )

runGen :: Gen r -> IO (Either Error r, [Constraint])
runGen = fmap f . runWriterT . flip runStateT 0 . runExceptT . unGen
  where
    f ((e, _), cs) = (e, cs)
