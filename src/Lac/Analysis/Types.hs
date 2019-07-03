{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx()
  , latexCtx
  , freshCtx
  , rootCtx
  , lengthCtx
  , numVarsCtx
  , splitCtx
  , ctxEmpty
  , ppCtx
  , ppConstr
  , coeff
  , coeffs
  , vecCoeffsRev
  , isRankCoeff
  , augmentCtx
  , weakenCtx
  , returnCtx
  , Idx(..)
  , enumRankCoeffs
  , idx
  , eqReturnCtx

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
  , assert
  , liftIO

  , Rule
  , setRuleName
  , accumConstr
  , prove
  , conclude
  )
  where

import           Control.Monad.State.Strict.Ext
import           Data.Bound
import           Data.Expr.Typed                (Typed)
import           Data.Term.Pretty
import           Data.Type
import           Lac.Analysis.ProofTree
import           Lac.Analysis.RuleName
import           Lac.Analysis.Types.Coeff
import           Lac.Analysis.Types.Constraint
import           Lac.Analysis.Types.Ctx
import           Latex

import           Control.Monad.Except
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Basic types

-- do not export `nullCtx`
nullCtx :: Ctx
nullCtx = Ctx 0 mempty mempty

rootCtx :: Ctx
rootCtx = nullCtx { ctxId = -1 }

freshCtx :: Gen Ctx
freshCtx = Ctx <$> fresh <*> pure mempty <*> pure mempty

countTrees :: [(Text, Type)] -> Int
countTrees = length . trees

trees :: [(Text, Type)] -> [(Text, Type)]
trees = filter (\(_, ty) -> ty == tyTree)

lengthCtx :: Ctx -> Int
lengthCtx = countTrees . M.toList . ctxVariables

numVarsCtx :: Ctx -> Int
numVarsCtx Ctx{..} = length . M.toList $ ctxVariables

augmentCtx :: Bound -> Ctx -> [(Text, Type)] -> Gen Ctx
augmentCtx bound ctx@Ctx{..} xs =
  do
    let xs' = M.toList ctxVariables ++ xs

    astCoefficient <- fresh >>= \i -> return (AstIdx, Coeff i)

    rankCoefficients <-
      mapM
        (\(x, _) -> fresh >>= \i -> return (IdIdx x, Coeff i))
        (trees xs')

    vecCoefficients <-
      let c = countTrees xs'
      in
      if c > 0
        then
          mapM
            (\vec -> fresh >>= \i -> return (VecIdx vec, Coeff i))
            (vectors bound (c + 1))
        else
          return []

    return $
      ctx { ctxVariables = M.fromList xs `M.union` ctxVariables
          , ctxCoefficients =
              M.fromList [astCoefficient]
                `M.union` M.fromList rankCoefficients
                `M.union` M.fromList vecCoefficients
          }

splitCtx :: Bound -> Ctx -> [Text] -> Gen ([(Text, Type)], Ctx)
splitCtx bound ctx xs = go ctx xs []
  where
    go :: Ctx -> [Text] -> [(Text, Type)] -> Gen ([(Text, Type)], Ctx)
    go ctx@Ctx{..} [] acc =
      do
        q <- freshCtx
        q' <- augmentCtx bound q (M.toList ctxVariables)
        return (reverse acc, q')
    go ctx@Ctx{..} (x:xs) acc =
      case M.updateLookupWithKey (const (const Nothing)) x ctxVariables of
        (Just ty, m) ->
          let ctx' = ctx { ctxVariables = m }
          in
          go ctx' xs ((x, ty) : acc)
        _ -> throwError . AssertionFailed $ "splitCtx: variable " <> x <> " not found in context"

weakenCtx :: Bound -> Ctx -> [Text] -> Gen ((Text, Type), Ctx)
weakenCtx u q@Ctx{..} xs =
  case (M.toList . deleteAll xs) ctxVariables of
    (y, _) : _ -> splitCtx u q [y] >>=
                    \case
                      ([(y, t)], q') -> return ((y, t), q')
                      _              -> throwError $ AssertionFailed "weakenCtx: unexpected splitCtx behavior"
    _ -> throwError $ AssertionFailed "weakenCtx: cannot weaken"

deleteAll :: Ord k => [k] -> Map k v -> Map k v
deleteAll [] m = m
deleteAll (x:xs) m =
  let m' = M.delete x m
  in
  deleteAll xs m'

ppCtx :: Ctx -> Text
ppCtx Ctx{..} =
  "variables: "
    <> T.intercalate ", " (map ppVar . M.toList $ ctxVariables)
    <> "\n"
    <> "coefficients:\n"
    <> T.intercalate "\n" (map ppCoeff . M.toList $ ctxCoefficients)

ppVar :: (Text, Type) -> Text
ppVar (x, ty) = x <> " : " <> ppTerm' ty

ppCoeff :: (Idx, Coeff) -> Text
ppCoeff (idx, (Coeff i)) = T.pack (show i) <> ": " <> c
  where
    c = case idx of
          IdIdx x    -> "q(" <> x <> ")"
          VecIdx vec -> "q(" <> T.intercalate ", " (map (T.pack . show) vec) <> ")"
          AstIdx     -> "q*"

ppConstr :: Constraint -> Text
ppConstr (CEq lhs rhs) = ppCExpr lhs <> " = " <> ppCExpr rhs
  where
    ppCExpr (CAtom (Coeff i)) = T.pack (show i)
    ppCExpr (CSum es) = T.intercalate " + " (map ppCExpr es)

coeff :: Ctx -> Idx -> Gen Coeff
coeff ctx idx =
  case coeff' ctx idx of
    Just c -> return c
    Nothing -> throwError . AssertionFailed $ "coefficient for index " <> T.pack (show idx) <> " not found"

coeff' :: Ctx -> Idx -> Maybe Coeff
coeff' Ctx{..} idx = M.lookup idx ctxCoefficients

coeffs :: Ctx -> (Idx -> Bool) -> [(Idx, Coeff)]
coeffs Ctx{..} p = filter (p . fst) . M.toList $ ctxCoefficients

isRankCoeff :: Idx -> Bool
isRankCoeff (IdIdx _) = True
isRankCoeff _         = False

idx :: Int -> Ctx -> Gen Idx
idx i Ctx{..} =
  case drop (i - 1) (trees . M.toList $ ctxVariables) of
    (x, _) : _ -> return $ IdIdx x
    _          -> throwError . AssertionFailed $ "coefficient for index " <> T.pack (show i) <> " not found"

vecCoeffsRev :: Ctx -> ([Int] -> Bool) -> [(Idx, Coeff)]
vecCoeffsRev Ctx{..} q = mapMaybe p . M.toList $ ctxCoefficients
  where
    p c@(VecIdx vec, _) | q (reverse vec) = Just c
    p _                                   = Nothing

enumRankCoeffs :: Ctx -> [(Idx, Coeff)]
enumRankCoeffs Ctx{..} = filter (p . fst) . M.toList $ ctxCoefficients
  where
    p (IdIdx _) = True
    p _         = False

returnCtx :: Bound -> Gen Ctx
returnCtx bound =
  do
    ctx <- freshCtx
    vecCoefficients <-
      mapM
        (\vec -> fresh >>= \i -> return (VecIdx vec, Coeff i))
        (vectors bound 2)
    i <- fresh
    return $
      ctx { ctxVariables = mempty
          , ctxCoefficients =
              M.singleton AstIdx (Coeff i)
              `M.union` M.fromList vecCoefficients
          }

eqReturnCtx :: Ctx -> Ctx -> Gen Bool
eqReturnCtx q r =
  case (coeff' q AstIdx, coeff' r AstIdx) of
    (Just c, Just d) -> do
      tellConstr [CEq (CAtom c) (CAtom d)]
      return True
    (Nothing, Nothing) -> return False
    _ -> throwError $ AssertionFailed "eqReturnCtx: bad contexts"

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

assert :: Bool -> Text -> Gen ()
assert p s =
  if p then return ()
       else throwError (AssertionFailed s)

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
    unGen :: ExceptT Error (StateT GenState (WriterT Output IO)) a
  }
  deriving (
    Functor
  , Applicative
  , Monad
  , MonadError Error
  , MonadState GenState
  , MonadWriter Output
  , MonadIO
  )

data GenState
  = GenState {
    gsFresh                :: Int
  , gsProofTreeRuleName    :: Maybe RuleName
  , gsProofTreeConstraints :: [Constraint]
  , gsProofTreeSubtrees    :: [ProofTree]
  }
  deriving Show

initState :: GenState
initState = GenState 0 Nothing mempty mempty

type Rule = Ctx -> Typed -> Gen ProofTree

setRuleName :: Text -> Gen ()
setRuleName n = do
  liftIO $ print n
  s@GenState{..} <- get
  if gsProofTreeRuleName /= Nothing
    then throwError $ AssertionFailed "rule name can only be set once"
    else
      let s' = s { gsProofTreeRuleName = Just (RuleName n) }
      in
      put s'

accumConstr :: [Constraint] -> Gen ()
accumConstr cs =
  modify $ \s@GenState{..} -> s { gsProofTreeConstraints = gsProofTreeConstraints ++ cs }

prove :: Rule -> Ctx -> Typed -> Gen Ctx
prove dispatch q e =
  do
    -- save current state
    saved <- get
    modify $ \s@GenState{..} -> initState { gsFresh = gsFresh }
    -- dispatch nested rule application
    t@ProofTree{..} <- dispatch q e
    -- recover state w/ updated "fresh" value
    modify $ \s@GenState{..} -> saved { gsFresh             = gsFresh
                                      , gsProofTreeSubtrees = t : gsProofTreeSubtrees
                                      }
    let (_, _, q') = ptConclusion
    return q'

conclude :: Ctx -> Typed -> Ctx -> Gen ProofTree
conclude q e q' =
  do
    s@GenState{..} <- get
    case gsProofTreeRuleName of
      Just n -> do
        put $ s { gsProofTreeRuleName    = Nothing
                , gsProofTreeConstraints = []
                , gsProofTreeSubtrees    = []
                }
        return $
          ProofTree
            (q, e, q')
            n
            gsProofTreeConstraints
            gsProofTreeSubtrees
      Nothing ->
        throwError $ AssertionFailed "cannot conclude w/o set rule name"

instance HasFresh GenState where
  getFresh GenState{..} = gsFresh
  putFresh i s = s { gsFresh = i }

runGen :: Gen r -> IO (Either Error r, Output)
runGen = fmap f . runWriterT . flip runStateT initState . runExceptT . unGen
  where
    f ((e, _), cs) = (e, cs)

tellConstr :: [Constraint] -> Gen ()
tellConstr cs = tell
  Output { outConstraints = cs
         , outEqs = []
         , outLog = []
         }
