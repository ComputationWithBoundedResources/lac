{-# LANGUAGE OverloadedStrings #-}

module Lac.Analysis.Rules.Common (
    module E

  , nil
  , hole
  ) where

import           Data.Bound             as E
import           Data.Expr.Typed        as E
import           Data.List.Ext          as E (equal)
import           Data.Text              as E (Text)
import           Data.Type              as E
import           Lac.Analysis.ProofTree as E
import           Lac.Analysis.RuleName  as E
import           Lac.Analysis.Types     as E
import           Lac.Analysis.Types.Ctx as E (astId, costId, trees)

import           Control.Monad          as E (forM, forM_, when)
import           Data.Default           as E (def)
import qualified Data.Text.IO           as T

nil :: Typed
nil = TyLit TyLNil

hole :: Typed
hole = TyVar "_"
