{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx()
  , ctxName
  , freshCtx
  , rootCtx
  , coeff
  , coeffs
  , augmentCtx
  , returnCtx
  , Idx(..)
  , enumRankCoeffs

  , Error(..)

  , Output(..)
  , outEq

  , Constraint(..)
  , CExpr(..)

  , Gen (..)
  , runGen
  , tell
  , tellConstr
  , throwError
  , liftIO
  )
  where

import           Control.Monad.State.Strict.Ext
import           Data.Bound
import           Data.Type
import           Latex

import           Control.Monad.Except
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as M
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Basic types

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

ctxName :: Ctx -> Text
ctxName Ctx{..} = "Q_{" <> T.pack (show ctxId) <> "}"

data Idx
  = AstIdx
  | IdIdx Text
  | VecIdx [Int]
  deriving (Eq, Ord, Show)

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

coeff :: Ctx -> Idx -> Gen Coeff
coeff Ctx{..} idx =
  case M.lookup idx ctxCoefficients of
    Just c -> return c
    Nothing -> throwError . AssertionFailed $ "coefficient for index " <> T.pack (show idx) <> " not found"

coeffs :: Ctx -> (Idx -> Bool) -> [(Idx, Coeff)]
coeffs Ctx{..} p = filter (p . fst) . M.toList $ ctxCoefficients

enumRankCoeffs :: Ctx -> [(Idx, Coeff)]
enumRankCoeffs Ctx{..} = filter (p . fst) . M.toList $ ctxCoefficients
  where
    p (IdIdx _) = True
    p _         = False

enumTreeVars :: Ctx -> [(Text, Type)]
enumTreeVars Ctx{..} = filter (p . snd) . M.toList $ ctxVariables
  where
    p t | t == tyTree = True
        | otherwise   = False

augmentCtx :: Bound -> [(Text, Type)] -> Bool -> Ctx -> Gen Ctx
augmentCtx bound vars ast ctx =
  do
    astCoefficient <-
      if ast
        then fresh >>= \i -> return [(AstIdx, Coeff i)]
        else return []
    rankCoefficients <-
      mapM
        (\(x, _) -> fresh >>= \i -> return (IdIdx x, Coeff i))
        vars
    vecCoefficients <-
      mapM
        (\vec -> fresh >>= \i -> return (VecIdx vec, Coeff i))
        (vectors bound (length vars + 1))
    return $
      ctx { ctxVariables = M.fromList vars
          , ctxCoefficients =
            M.fromList astCoefficient
            `M.union` M.fromList rankCoefficients
            `M.union` M.fromList vecCoefficients
          }

returnCtx :: Bound -> Int -> Bool -> Gen Ctx
returnCtx bound nvars ast =
  do
    ctx <- freshCtx
    astCoefficient <-
      if ast
        then fresh >>= \i -> return [(AstIdx, Coeff i)]
        else return []
    vecCoefficients <-
      mapM
        (\vec -> fresh >>= \i -> return (VecIdx vec, Coeff i))
        (vectors bound (nvars + 1))
    return $
      ctx { ctxVariables = mempty
          , ctxCoefficients =
              M.fromList astCoefficient
            `M.union` M.fromList vecCoefficients
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
      else T.intercalate ", " $ map f . M.toList $ ctxVariables
    where
      f (x, ty) = x <> ": TODO"

-- * Execution

data Error
  = NotImplemented Text
  | NotApplicable Text
  | AssertionFailed Text
  deriving (Eq, Show)

data Constraint
  = CEq CExpr CExpr
  deriving (Eq, Show)

data CExpr
  = CAtom Coeff
  | CSum [CExpr]
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
