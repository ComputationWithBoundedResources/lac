{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.State.Strict.Ext (
    module E
  , fresh
  , HasFresh(..)
  ) where

import           Control.Monad.State.Strict as E

class HasFresh a where
  getFresh :: a -> Int
  putFresh :: Int -> a -> a

instance HasFresh Int where
  getFresh = id
  putFresh = const

fresh :: (MonadState s m, HasFresh s) => m Int
fresh = do
  s <- get
  let i = getFresh s
  let i' = i + 1
  let s' = putFresh i' s
  put s'
  return i'
