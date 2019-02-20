module Main where

import           Data.Expr

import qualified Data.Text.IO       as T
import           System.Environment
import           System.Exit
import           Text.Parsec        (parse)
import Debug.Trace

main :: IO ()
main =
  do
    args <- getArgs
    mapM_ process args

process :: FilePath -> IO ()
process path =
  do
    r <- parse prog path <$> T.readFile path
    case r of
      Left _ -> exitFailure
      Right decls -> do
        --traceShowM decls
        --mapM_ (T.putStrLn . ppDecl) decls
        mapM_ (T.putStrLn . ppDecl . letNF') decls

  where
    letNF' (Decl x xs e) =
      let e' = (toLetNF e)
      in
      Decl x xs e'
