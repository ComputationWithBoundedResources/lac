{-# LANGUAGE RecordWildCards #-}

module Data.TypeAnn.TypeSig where

import           Data.Type
import           Data.TypeAnn.TypeAnn

import           Data.Text            (Text)

data TypeSig
  = TypeSig {
    tsName :: Text
  , tsType :: Type
  , tsAnn  :: TypeAnn
  }
  deriving (Eq, Show)
