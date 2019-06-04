{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Expr.Parser  (expr)
import           Data.Expr.Typed   (ppTyped)
import           Data.Term         (T (..))
import qualified Data.Text.IO      as T
import           Data.Type
import           Lac.TypeInference

import           Text.Parsec

var = V

env :: Env
env =
  [ (var "incr", tyFun tyNat tyNat)
  ]

getExpr = (\(Right x) -> x) . parse expr "<stdin>" <$> getLine

main = undefined
