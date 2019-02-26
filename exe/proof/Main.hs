module Main where

import           Data.Expr
import           Lac.Analysis
import           Lac.Analysis.Types
import           Lac.TypeInference

import           Data.List.NonEmpty
import           System.Process

main :: IO ()
main = do
  let e = Match (Var "t") cs
        where
          cs = (PNil, Lit LNil) :| [(PNode "cl" "c" "cr", e1)]
          e1 = Ite (Var "x") (Var "TT") (Var "FF")
  let q = (nullCtx "Q") { ctxMembers = [ ("t", AnTy (tyTree tyAbs) ())
                                       , ("e1", AnTy tyBool ())
                                       , ("x", AnTy tyBool ())
                                       , ("TT", AnTy tyBool ())
                                       , ("FF", AnTy tyBool ())
                                       ]
                        }
  writeProof "out.tex" q e
  system "pdflatex out"
  return ()
