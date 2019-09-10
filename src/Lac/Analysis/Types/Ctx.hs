{-# LANGUAGE LambdaCase        #-}
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
import           Data.Vector              (Vector)
import qualified Data.List as L

data Ctx
  = Ctx {
    -- | Context identifier, i.e. 2 for context @Q_2@
    ctxId           :: Int
    -- | Coefficients in context
    -- | Variables in context
  , ctxCoefficients :: Map Idx Coeff
  , ctxVariables    :: [(Text, Type)]
  }
  deriving (Eq, Show)

length :: Ctx -> Int
length Ctx{..} = L.length . filter (isTyTree . snd) $ ctxVariables

-- TODO: "empty" should mean "does not contain variables of type tree"
-- TODO: rename to `empty` or `null`; add note that module should be imported qualified
ctxEmpty :: Ctx -> Bool
ctxEmpty Ctx{..} = null ctxVariables

ctxVars :: Ctx -> Set Text
ctxVars Ctx{..} = S.fromList . map fst $ ctxVariables

data Idx
  = RankIdx !Int
  | VecIdx !(Vector Int)
  deriving (Eq, Ord, Show)

isRankIdx :: Idx -> Bool
isRankIdx =
  \case
    RankIdx _ -> True
    _         -> False

astId :: Text
astId = "*"

astIdx :: Idx
astIdx = RankIdx 1

costId :: Text
costId = "+"

ptCoefficients :: Ctx -> [Coeff]
ptCoefficients Ctx{..} = map snd . M.toList $ ctxCoefficients

trees :: Ctx -> [Text]
trees Ctx{..} = map fst . filter (isTyTree . snd) $ ctxVariables

nonTrees :: Ctx -> [(Text, Type)]
nonTrees = filter (not . isTyTree . snd) . ctxVariables

-- * Pretty-printing

latexCtx :: Ctx -> Text
latexCtx Ctx{..} = varCtx <> "|Q_{" <> T.pack (show ctxId) <> "}"
  where
    varCtx | null vars = "\\varnothing"
           | otherwise = T.intercalate ", " vars
    vars = Prelude.map var ctxVariables
    var (x, ty) = latexVar x <> ": " <> latexType ty

-- TODO: show type
latexRetCtx :: Ctx -> Text
latexRetCtx Ctx{..} = "\\Box|Q_{" <> T.pack (show ctxId) <> "}"
