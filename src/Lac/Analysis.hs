{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      hiding (expr)
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Types
import           Latex

import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Rules

type Rule a = Ctx -> Expr -> Ctx -> Gen a

-- | Dispatch rule
dispatch :: Rule ProofTree
dispatch ctx expr ctx' = do
  gen <- case expr of
    Abs _ _   -> throwError $ NotImplemented "abstraction"
    Lit _     -> throwError $ NotImplemented "literal"
    Cmp _ _ _ -> throwError $ NotImplemented "comparison"
    Ite _ _ _ -> throwError $ NotImplemented "if-then-else"
    Let _ _ _ -> throwError $ NotImplemented "let"
    App _ _   -> throwError $ NotImplemented "application"
    Match _ _ -> throwError $ NotImplemented "match"
    Var _     -> throwError $ NotImplemented "var"
  gen ctx expr ctx'

mkConcl :: Ctx -> Expr -> Ctx -> Text
mkConcl ctxL expr ctxR =
  T.unwords
    [ latex ctxL
    , "\\vdash"
    , latex expr
    , ": "
    , latex ctxR
    ]

writeProof :: FilePath -> Ctx -> Expr -> IO ()
writeProof path ctx expr =
  let q' = rootCtx
      f = dispatch ctx expr q'
  in
  runGen f >>=
    \(e, Output{..}) ->
      let eqs = outEqs
      in
      case e of
        Left e -> print e
        Right p ->
          let content = concat [
                  "\\documentclass[12pt,preview]{standalone}"
                , "\\usepackage{amssymb}"
                , "\\usepackage{amsmath}"
                , "\\usepackage{proof}"
                , "\\begin{document}"
                , T.unpack (latex p)
                , "\n\n\n"
                , T.unpack eqAry
                , "\\end{document}"
                ]
              eqAry
                | null eqs = ""
                | otherwise =
                    T.concat [
                        "\\begin{align*}\n"
                      , T.intercalate "\\\\\n" (map (\(a, b) -> a <> " &= " <> b) eqs)
                      , "\\end{align*}\n"
                      ]
          in
          do writeFile path content
             print eqs

nameExpr :: Expr -> Gen Expr
nameExpr expr
  | isSimple expr = return expr
  | otherwise = do
      x <- freshVar "e"
      tell $ outEq x (latex expr)
      return (Var x)

freshVar :: Text -> Gen Text
freshVar prefix = do
  i <- fresh
  return $ prefix <> "_{" <> T.pack (show i) <> "}"
