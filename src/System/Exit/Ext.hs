module System.Exit.Ext (
    module E
  , dieT
  ) where

import           Data.Text    (Text)
import qualified Data.Text.IO as T
import           System.Exit  as E

dieT :: Text -> IO ()
dieT m = T.putStrLn m >> exitFailure
