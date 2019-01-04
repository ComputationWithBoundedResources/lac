{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis where

import           Data.Expr          hiding (expr)
import           Lac.Analysis.Types

analyzeExpr :: Expr -> IO (Either Error (), [Constraint])
analyzeExpr expr = runGen $
  case expr of
    Abs _ _ -> throwError $ NotImplemented "abstraction"
    Lit _ -> throwError $ NotImplemented "literal"
    Cmp _ _ _ -> throwError $ NotImplemented "comparison"
    Ite _ _ _ -> throwError $ NotImplemented "if-then-else"
    Let _ _ _ -> throwError $ NotImplemented "let"
    App _ _ -> throwError $ NotImplemented "application"
    Match _ _ -> throwError $ NotImplemented "match"
    Var _ -> throwError $ NotImplemented "variable"
