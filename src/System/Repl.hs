module System.Repl where

import           Control.Exception          (try)
import           Control.Monad              (when)
import           Control.Monad.State.Strict
import           System.IO                  (hFlush, stdout)
import           System.IO.Error            (ioeGetErrorString, isEOFError)

repl :: String -> s -> (String -> StateT s IO Bool) -> IO s
repl prompt s f = snd <$> runStateT go s
  where
    go = do
      liftIO $ putStr prompt
      liftIO $ hFlush stdout
      result <- liftIO $ try getLine
      case result of
        Left e
          | isEOFError e -> return ()
          | otherwise    -> liftIO $ putStrLn (ioeGetErrorString e)
        Right line       -> f line >>= flip when go

multiline :: String -> String -> StateT t IO [String]
multiline prompt end = fmap reverse . lift $ repl prompt [] $ \line ->
  if line == end
    then return False
    else modify ((:) line) >> return True
