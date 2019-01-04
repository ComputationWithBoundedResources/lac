{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lac.Analysis where

import Control.Monad.Except
import Control.Monad.Writer

data Error
  = Error
  deriving (Eq, Show)

data Constraint
  = CEq Expr Expr
  | CLe Expr Expr
  deriving (Eq, Show)

data Expr
  = EConst Int
  | ESum [Expr]
  | EProd [Expr]
  deriving (Eq, Show)

-- | Constraint-generating computation
newtype Gen a = Gen {
    unGen :: ExceptT Error (WriterT [Constraint] IO) a
  }
  deriving (
    Functor
  , Applicative
  , Monad
  , MonadWriter [Constraint]
  , MonadError Error
  , MonadIO
  )

runGen :: Gen r -> IO (Either Error r, [Constraint])
runGen = runWriterT . runExceptT . unGen
