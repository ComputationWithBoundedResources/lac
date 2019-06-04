{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx(..)
  , freshCtx
  , rootCtx
  , insert
  , insertCtx
  , insertAstCtx
  , insertVecCtx
  , augmentCtx
  , splitCtx
  , coeffCtx

  , Error(..)

  , Output(..)
  , outEq

  , Constraint(..)
  , Gen (..)
  , runGen
  , tell
  , tellConstr
  , throwError
  )
  where

import           Control.Monad.State.Strict.Ext
import           Data.List.Helpers
import           Data.Type
import           Lac.TypeInference
import           Latex

import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.List                      (intercalate)
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Basic types

data Ctx
  = Ctx {
    ctxId      :: Int
  , ctxName    :: Text
  , ctxMembers :: [(Text, Text)] -- is this field needed?
  , ctxVec     :: [([Int], Text)]
  , ctxAst     :: Maybe Text
  }
  deriving (Eq, Show)

-- do not export `nullCtx`
nullCtx :: Ctx
nullCtx = Ctx 0 mempty mempty mempty mempty

rootCtx :: Ctx
rootCtx = nullCtx { ctxId = -1
                  , ctxName = "Γ_{root}"
                  }

freshCtx :: Gen Ctx
freshCtx =
  do
    idx <- fresh
    return $
      Ctx idx (mkNam idx) mempty mempty mempty
  where
    mkNam i | i < 10    = "Γ_" <> T.pack (show i)
            | otherwise = "Γ_{" <> T.pack (show i) <> "}"

insert :: Text -> Ctx -> Gen Ctx
insert x ctx = snd <$> insertCtx x ctx

insertCtx :: Text -> Ctx -> Gen (Text, Ctx)
insertCtx x ctx@Ctx{..} =
  do
    i <- fresh
    let new = ctxName <> "_{" <> T.pack (show i) <> "}"
    let ctx' = ctx { ctxMembers = (x, new) : ctxMembers }
    q <- coeffCtx x ctx'
    return (q, ctx')

insertAstCtx :: Ctx -> Gen (Text, Ctx)
insertAstCtx ctx@Ctx{..} =
  do
    -- TODO: fail if "asterisk" coefficient is already set
    i <- fresh
    let new = ctxName <> "_\\ast"
    let ctx' = ctx { ctxAst = Just new }
    return (new, ctx')

insertVecCtx :: [Int] -> Ctx -> Gen (Text, Ctx)
insertVecCtx vec ctx@Ctx{..} =
  do
    i <- fresh
    let new = ctxName <> "_{(" <> T.pack (intercalate "," (map show vec)) <> ")}"
    let ctx' = ctx { ctxVec = (vec, new) : ctxVec }
    q <- vecCtx vec ctx'
    return (q, ctx')

vecCtx :: [Int] -> Ctx -> Gen Text
vecCtx vec ctx@Ctx{..} =
  case lookup vec ctxVec of
    Nothing -> error $ "coefficient for vector " <> show vec <> " does not appear in context"
    Just q -> return q

instance Latex Ctx where
  latex Ctx{..} =
    if null ctxMembers
      then "\\varnothing"
      else T.intercalate ", " $ map f ctxMembers
    where
      f (x, ty) = x <> ": TODO" -- <> ppAnTy ty

      -- TODO: format type annotation
      ppAnTy :: AnTy -> Text
      ppAnTy (AnTy ty _) = latex ty

data AnTy
  = AnTy {
    anTyType       :: Type
  , anTyAnnotation :: ()
  }
  deriving (Eq, Show)

splitCtx :: Text -> Ctx -> Gen (Text, Ctx)
splitCtx x ctx@Ctx{..} =
  case lookup x ctxMembers of
    Just ty -> let ctx' = ctx { ctxMembers = delete' x ctxMembers }
               in
               return (ty, ctx')
    Nothing -> throwError $ AssertionFailed $ "variable `" <> x <> "` does not appear in context"

coeffCtx :: Text -> Ctx -> Gen Text
coeffCtx x ctx@Ctx{..} =
  case lookup x ctxMembers of
    Nothing -> error $ "variable `" <> T.unpack x <> "` not found in context"
    Just x -> return x

augmentCtx :: [(Text, Text)] -> Ctx -> Gen Ctx
augmentCtx xs ctx@Ctx{..} =
  do
    name <- freshCtxName
    return $
      ctx {
        ctxName    = name
      , ctxMembers = xs ++ ctxMembers
      }

freshCtxName :: Gen Text
freshCtxName = do
  i <- fresh
  return (T.pack $ "C_{" <> show i <> "}")

-- * Execution

data Error
  = NotImplemented Text
  | NotApplicable Text
  | AssertionFailed Text
  deriving (Eq, Show)

data Constraint
  = CEq Text Text
  deriving (Eq, Show)

data Output
  = Output {
      outConstraints :: [Constraint]
    , outLog         :: [Text]
    , outEqs         :: [(Text, Text)]
  }
  deriving (Eq, Show)

outEq :: Text -> Text -> Output
outEq lhs rhs = Output mempty mempty [(lhs, rhs)]

instance Semigroup Output where
  Output xs1 ys1 zs1 <> Output xs2 ys2 zs2 =
    Output (xs1 <> xs2) (ys1 <> ys2) (zs1 <> zs2)

instance Monoid Output where
  mempty = Output mempty mempty mempty

newtype Gen a = Gen {
    unGen :: ExceptT Error (StateT Int (WriterT Output IO)) a
  }
  deriving (
    Functor
  , Applicative
  , Monad
  , MonadError Error
  , MonadState Int
  , MonadWriter Output
  , MonadIO
  )

runGen :: Gen r -> IO (Either Error r, Output)
runGen = fmap f . runWriterT . flip runStateT 0 . runExceptT . unGen
  where
    f ((e, _), cs) = (e, cs)

tellConstr :: [Constraint] -> Gen ()
tellConstr cs = tell
  Output { outConstraints = cs
         , outEqs = []
         , outLog = []
         }
