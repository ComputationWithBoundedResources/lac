{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lac.Analysis.Types (
    Error(..)
  , Constraint(..)
  , CExpr(..)

  , Gen(..)
  , runGen
  , tell
  , throwError
  )
  where

import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Text            (Text)

data Error
  = NotImplemented Text
  deriving (Eq, Show)

data Constraint
  = CEq CExpr CExpr
  | CLe CExpr CExpr
  deriving (Eq, Show)

data CExpr
  = EConst Int
  | ESum [CExpr]
  | EProd [CExpr]
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
