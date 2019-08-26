{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis.Types.Ctx where

import           Data.Type
import           Lac.Analysis.Types.Coeff
import           Lac.PP

import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T

data Ctx
  = Ctx {
    -- | Context identifier, i.e. 2 for context @Q_2@
    ctxId           :: Int
    -- | Coefficients in context
    -- | Variables in context
  , ctxCoefficients :: Map Idx Coeff
  , ctxVariables    :: Map Text Type
  }
  deriving (Eq, Show)

latexCtx :: Ctx -> Text
latexCtx Ctx{..} = varCtx <> "|Q_{" <> T.pack (show ctxId) <> "}"
  where
    varCtx | null vars = "\\varnothing"
           | otherwise = T.intercalate ", " vars
    vars = Prelude.map var . M.toList $ ctxVariables
    var (x, ty) = latexVar x <> ": " <> latexType ty

-- TODO: show type
latexRetCtx :: Ctx -> Text
latexRetCtx Ctx{..} = "\\Box|Q_{" <> T.pack (show ctxId) <> "}"

ctxEmpty :: Ctx -> Bool
ctxEmpty Ctx{..} = M.null ctxVariables

ctxVars :: Ctx -> Set Text
ctxVars Ctx{..} = S.fromList (map fst . M.toList $ ctxVariables)

data Idx
  = IdIdx Text
  | VecIdx (Set (Text, Int))
  deriving (Eq, Ord, Show)

astId :: Text
astId = "*"

astIdx :: Idx
astIdx = IdIdx astId

costId :: Text
costId = "+"

costIdx :: Idx
costIdx = IdIdx costId

ptCoefficients :: Ctx -> [Coeff]
ptCoefficients Ctx{..} = map snd . M.toList $ ctxCoefficients

trees :: Ctx -> [Text]
trees Ctx{..} = map fst . filter (isTyTree . snd) . M.toList $ ctxVariables

vecIdx :: [(Text, Int)] -> Idx
vecIdx = VecIdx . S.fromList
