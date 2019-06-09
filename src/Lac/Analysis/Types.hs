{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx(..)
  , freshCtx
  , rootCtx

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
    ctxId           :: Int
  , ctxName         :: Text
  , ctxCoefficients :: [(Idx, Coeff)]
  , ctxMembers      :: [(Text, Type)]
  }
  deriving (Eq, Show)

data Idx
  = IdIdx Text
  | VecIdx [Int]
  | AstIdx
  deriving (Eq, Show)

data Coeff
  = Coeff {
    coeffId   :: Int
  , coeffName :: Text
  }
  deriving (Eq, Show)

-- do not export `nullCtx`
nullCtx :: Ctx
nullCtx = Ctx 0 mempty mempty mempty

rootCtx :: Ctx
rootCtx = nullCtx { ctxId = -1
                  , ctxName = "Γ_{root}"
                  }

freshCtx :: Gen Ctx
freshCtx =
  do
    idx <- fresh
    return $
      Ctx idx (mkNam idx) mempty mempty
  where
    mkNam i | i < 10    = "Γ_" <> T.pack (show i)
            | otherwise = "Γ_{" <> T.pack (show i) <> "}"

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
