module Control.Monad.Ext (
    module E
  , ifM
  ) where

import qualified Control.Monad as E

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM f e1 e2 = do
  p <- f
  if p
    then e1
    else e2
