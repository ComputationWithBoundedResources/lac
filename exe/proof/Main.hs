{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Expr
import           Lac.Analysis
import           Lac.Analysis.Types

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
                -- TODO: populate environment
                let ctx = rootCtx
                writeProof outPath ctx expr
                system $ "pdflatex " <> outPath
                exitSuccess
      _ ->
        do
          progName <- getProgName
          putStrLn $ "Usage: " <> progName <> " input.txt output.tex"
          exitFailure
