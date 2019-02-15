module Control.Monad.State.Ext (
    module E
  , fresh
  ) where

import           Control.Monad.State as E

fresh :: Monad m => StateT Int m Int
fresh = do
  i <- get
  let i' = i + 1
  put i'
  return i'
