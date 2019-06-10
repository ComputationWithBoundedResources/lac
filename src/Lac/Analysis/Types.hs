{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx(..)
  , ctxName
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
import           Data.Bound
import           Data.Type
import           Latex

import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Basic types

data Ctx
  = Ctx {
    -- | Context identifier, i.e. 2 for context @Q_2@
    ctxId           :: Int
    -- | Coefficients in context
    -- | Variables in context
  , ctxCoefficients :: [(Idx, Coeff)]
  , ctxVariables    :: [(Text, Type)]
  }
  deriving (Eq, Show)

ctxName :: Ctx -> Text
ctxName Ctx{..} = "Q_{" <> T.pack (show ctxId) <> "}"

data Idx
  = IdIdx Text
  | VecIdx [Int]
  | AstIdx
  deriving (Eq, Show)

data Coeff
  = Coeff {
    coeffId :: Int
  }
  deriving (Eq, Show)

-- do not export `nullCtx`
nullCtx :: Ctx
nullCtx = Ctx 0 mempty mempty

rootCtx :: Ctx
rootCtx = nullCtx { ctxId = -1 }

freshCtx :: Gen Ctx
freshCtx = Ctx <$> fresh <*> pure mempty <*> pure mempty

augmentCtx :: Bound -> [(Text, Type)] -> Ctx -> Gen Ctx
augmentCtx bound vars ctx =
  do
    rankCoefficients <-
      mapM
        (\(x, _) -> fresh >>= \i -> return (IdIdx x, Coeff i))
        vars
    vecCoefficients <-
      mapM
        (\vec -> fresh >>= \i -> return (VecIdx vec, Coeff i))
        (vectors bound (length vars + 1))
    return $
      ctx { ctxVariables = vars
          , ctxCoefficients = rankCoefficients ++ vecCoefficients
          }

vectors :: Bound -> Int -> [[Int]]
vectors (Bound bound) n = go n
  where
    range = [0..bound]

    go 0 = []
    go 1 = map (\x -> [x]) range
    go n = [i : xs | i <- range, xs <- go (n - 1)]

instance Latex Ctx where
  latex Ctx{..} =
    if null ctxVariables
      then "\\varnothing"
      else T.intercalate ", " $ map f ctxVariables
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
