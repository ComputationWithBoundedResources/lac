{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lac.Analysis.Types (
    Error(..)
  , Constraint(..)

  , Gen (..)
  , runGen
  , tell
  , throwError
  )
  where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Text                  (Text)

data Error
  = NotImplemented Text
  | NotApplicable Text
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
