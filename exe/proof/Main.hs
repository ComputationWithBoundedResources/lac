{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Expr
import           Data.Subst
import           Lac.Analysis
import           Lac.Analysis.Types
import           Lac.TypeInference

import           Data.List.NonEmpty
import           System.Environment
import           System.Exit
import           System.Process
import           Text.Parsec

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [inPath, outPath] ->
        do
          contents <- readFile inPath
          case parse expr inPath contents of
            Left _ -> exitFailure
            Right expr ->
              do
                let subst = ()
                -- TODO: populate environment
                let ctx = nullCtx "Q"
                writeProof outPath subst ctx expr
                system $ "pdflatex " <> outPath
                exitSuccess
      _ ->
        do
          progName <- getProgName
          putStrLn $ "Usage: " <> progName <> " input.txt output.tex"
          exitFailure
