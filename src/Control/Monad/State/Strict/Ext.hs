{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.State.Strict.Ext (
    module E
  , fresh
  ) where

import           Control.Monad.State.Strict as E

fresh :: MonadState Int m => m Int
fresh = do
  i <- get
  let i' = i + 1
  put i'
  return i'
