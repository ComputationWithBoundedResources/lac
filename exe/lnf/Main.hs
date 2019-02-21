{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Expr
import           Lac

import qualified Data.Text.IO       as T
import           System.Environment
import           System.Exit

main :: IO ()
main =
  do
    args <- getArgs
    mapM_ process args

process :: FilePath -> IO ()
process path =
  do
    r <- readProg path
    case r of
      Left _ -> exitFailure
      Right Prog{..} -> do
        --traceShowM decls
        --mapM_ (T.putStrLn . ppDecl) decls
        mapM_ (T.putStrLn . ppDecl . letNF') progDecls

  where
    letNF' (Decl x xs e) =
      let e' = (toLetNF e)
      in
      Decl x xs e'
