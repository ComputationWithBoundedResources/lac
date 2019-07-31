{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx()
  , latexCtx
  , emptyCtx
  , lengthCtx
  , numVarsCtx
  , splitCtx
  , splitCtx'
  , ctxEmpty
  , ctxVars
  , ppCtx
  , ppConstr

  , coeff
  , coeffs
  , VecSel(..)
  , selAll
  , forVec
  , forVec_

  , isRankCoeff
  , augmentCtx
  , weakenCtx
  , returnCtx
  , Idx(..)
  , astIdx
  , enumRankCoeffs
  , eqCtx

  , Error(..)

  , Output(..)
  , outEq

  , Constraint(..)
  , CExpr(..)

  , Gen (..)
  , runGen
  , tell
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
import           Data.List.Helpers              (enum)
import           Data.Term.Pretty
import           Data.Type
import           Lac.Analysis.ProofTree
import           Lac.Analysis.RuleName
import           Lac.Analysis.Types.Coeff
import           Lac.Analysis.Types.Constraint
import           Lac.Analysis.Types.Ctx
import           Latex

import           Control.Arrow                  (first)
import           Control.Monad.Except
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict.Ext            as M
import           Data.Maybe                     (mapMaybe)
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Basic types

freshCtx :: Gen Ctx
freshCtx = Ctx <$> fresh <*> pure mempty <*> pure mempty

emptyCtx :: Bound -> Gen Ctx
emptyCtx (Bound u) =
  do
    q <- freshCtx
    cs <- forM is $ \i -> fresh >>= \a -> return (i, Coeff a)
    return $ q { ctxCoefficients = M.fromList cs }
  where
    is = [VecIdx (S.singleton (costId, c)) | c <- [0..u]]

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
    let ts = trees xs'

    rankCoefficients <-
      mapM
        (\(x, _) -> fresh >>= \i -> return (IdIdx x, Coeff i))
        ts

    vecCoefficients <-
      mapM
        (\vec -> fresh >>= \i -> return (VecIdx (S.fromList vec), Coeff i))
        (vecs bound . ("+" :) . map fst $ ts)

    return $
      ctx { ctxVariables = M.fromList xs `M.union` ctxVariables
          , ctxCoefficients =
              M.fromList rankCoefficients
                `M.union` M.fromList vecCoefficients
          }

splitCtx :: Bound -> Ctx -> [Text] -> Gen ([(Text, Type)], Ctx)
splitCtx bound q xs = go q xs []
  where
    go :: Ctx -> [Text] -> [(Text, Type)] -> Gen ([(Text, Type)], Ctx)
    go ctx@Ctx{..} [] acc =
      do
        r <- emptyCtx bound
        r' <- augmentCtx bound r (M.toList ctxVariables)
        return (reverse acc, r')
    go ctx@Ctx{..} (y:ys) acc =
      case M.updateLookupWithKey (const (const Nothing)) y ctxVariables of
        (Just ty, m) ->
          let ctx' = ctx { ctxVariables = m }
          in
          go ctx' ys ((y, ty) : acc)
        _ ->
          throwError . AssertionFailed $ "splitCtx: variable " <> y <> " not found in context"

splitCtx' :: Bound -> Ctx -> Text -> Gen ((Text, Type), Ctx)
splitCtx' u q y =
  splitCtx u q [y] >>=
    \(xs, q') ->
      case xs of
        [(x, ty)] -> return ((x, ty), q')
        _ -> throwError . AssertionFailed $ "splitCtx': splitCtx returned /= 1 variable/type pair"

weakenCtx :: Bound -> Ctx -> [Text] -> Gen ((Text, Type), Ctx)
weakenCtx u q@Ctx{..} xs =
  case (M.toList . M.deleteAll xs) ctxVariables of
    (y, _) : _ -> splitCtx u q [y] >>=
                    \case
                      ([(y, t)], q') -> return ((y, t), q')
                      _              -> throwError $ AssertionFailed "weakenCtx: unexpected splitCtx behavior"
    _ -> throwError $ AssertionFailed "weakenCtx: cannot weaken"

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
          IdIdx "*"  -> "q*"
          IdIdx x    -> "q(" <> x <> ")"
          -- TODO: fix output
          VecIdx vec -> "q(" <> T.intercalate ", " (map (T.pack . show) . S.toList $ vec) <> ")"

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

data VecSel a
  = Accept a
  | Reject
  | Invalid Text
  deriving (Eq, Ord, Show)

selAll = Accept . id

forVec :: MonadError Error m => Ctx -> ([(Text, Int)] -> VecSel a) -> ((a, Coeff) -> m b) -> m [b]
forVec Ctx{..} p g =
  do
    xs <- foldM f [] . M.toList $ ctxCoefficients
    mapM g xs
  where
    f xs (VecIdx i, c) =
      let i' = S.toList i
      in
      case p i' of
        Accept j -> return $ (j, c) : xs
        Reject   -> return xs
        Invalid e -> throwError (AssertionFailed e)
    f xs _ = return xs

forVec_ :: MonadError Error m => Ctx -> ([(Text, Int)] -> VecSel a) -> ((a, Coeff) -> m b) -> m ()
forVec_ q f g = void (forVec q f g)

dropCtxVars :: Ctx -> [(Text, Int)] -> Maybe [(Text, Int)]
dropCtxVars Ctx{..} = Just . filter q
  where
    xs = map fst . filter p . M.toList $ ctxVariables

    p (_, ty) = ty == tyTree

    q ("+", _) = True
    q (x, v)   = x `notElem` xs

isRankCoeff :: Idx -> Bool
isRankCoeff (IdIdx _) = True
isRankCoeff _         = False

enumRankCoeffs :: Ctx -> [(Idx, Coeff)]
enumRankCoeffs Ctx{..} = filter (p . fst) . M.toList $ ctxCoefficients
  where
    p (IdIdx _) = True
    p _         = False

returnCtx :: Bound -> Gen Ctx
returnCtx b =
  emptyCtx b >>= \q ->
    augmentCtx b q [("*", tyTree)]

eqCtx :: Ctx -> Ctx -> Gen ()
eqCtx q r =
  let f = S.fromList . M.keys . ctxCoefficients
      kq = f q
      kr = f r
  in
  if kq /= kr
    then
      let m = "eqCtx: contexts differ (Q: " <> g kq <> "; R: " <> g kr <> ")"
          g = T.intercalate "," . map (T.pack . show) . S.toList
      in
      throwError (AssertionFailed m)
    else
      forM_ kq $ \i -> do
        qi <- coeff q i
        ri <- coeff r i
        accumConstr $ [CEq (CAtom qi) (CAtom ri)]

vecs :: Bound -> [Text] -> [[(Text, Int)]]
vecs (Bound u) xs =
  let vvs = enum u (length xs)
  in
  map (zip xs) vvs

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
      outLog         :: [Text]
    , outEqs         :: [(Text, Text)]
  }
  deriving (Eq, Show)

outEq :: Text -> Text -> Output
outEq lhs rhs = Output mempty [(lhs, rhs)]

instance Semigroup Output where
  Output ys1 zs1 <> Output ys2 zs2 =
    Output (ys1 <> ys2) (zs1 <> zs2)

instance Monoid Output where
  mempty = Output mempty mempty

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
    modify $ \s -> saved { gsFresh             = gsFresh s
                         , gsProofTreeSubtrees = gsProofTreeSubtrees saved ++ [t]
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
        throwError $ AssertionFailed "no rule name set"

instance HasFresh GenState where
  getFresh GenState{..} = gsFresh
  putFresh i s = s { gsFresh = i }

runGen :: Gen r -> IO (Either Error r, Output)
runGen = fmap f . runWriterT . flip runStateT initState . runExceptT . unGen
  where
    f ((e, _), cs) = (e, cs)
