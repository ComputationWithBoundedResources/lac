module Lac.Prog where

import           Data.Expr
import           Lac.Eval.Value

import           Data.Map.Strict (Map)
import           Data.Text       (Text)

data Prog
  = Prog {
    progDecls :: [Decl]
  , progEnv   :: Map Text Value
  }
  deriving (Eq, Show)
