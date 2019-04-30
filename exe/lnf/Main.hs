{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Expr
import           Lac

import qualified Data.Text.IO           as T
import           System.Environment.Ext
import           System.Exit

main :: IO ()
main =
  do
    (_, args) <- partitionArgs <$> getArgs
    mapM_ process args

process :: FilePath -> IO ()
process path =
  do
    r <- readProg path
    case r of
      Left e ->
        do
          print e
          exitFailure
      Right Prog{..} ->
        mapM_ (T.putStrLn . ppDecl . letNF') progDecls

  where
    letNF' (Decl x xs e) =
      let e' = (toLetNF e)
      in
      Decl x xs e'
