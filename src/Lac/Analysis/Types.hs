{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Lac.Analysis.Types (
    Ctx()
  , latexCtx
  , emptyCtx
  , numVarsCtx
  , splitCtx
  , splitCtx'
  , ctxEmpty
  , ctxVars
  , ppCtx
  , ppConstr

  , coeff
  , coeffs

  , varIdx
  , vecIdx

  , freshCtx
  , augmentCtx
  , weakenCtx
  , returnCtx
  , copyCtx
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
import           Data.List.Ext                  (enum)
import           Data.Term.Pretty
import           Data.Type
import           Lac.Analysis.ProofTree
import           Lac.Analysis.RuleName
import           Lac.Analysis.Types.Coeff
import           Lac.Analysis.Types.Constraint
import           Lac.Analysis.Types.Ctx         hiding (length)
import qualified Lac.Analysis.Types.Ctx         as Ctx
import           Latex

import           Control.Monad.Except
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer.Strict
import qualified Data.List.Ext                  as L
import qualified Data.Map.Strict.Ext            as M
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import qualified Data.Vector.Mutable            as MV

import           Debug.Trace

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
    is = [VecIdx (V.singleton c) | c <- [0..u]]

numVarsCtx :: Ctx -> Int
numVarsCtx Ctx{..} = length ctxVariables

augmentCtx :: Bound -> Ctx -> [(Text, Type)] -> Gen Ctx
augmentCtx (Bound u) ctx@Ctx{..} xs =
  do
    let ts = trees ctx ++ (map fst . filter (isTyTree . snd) $ xs)
    let nts = length ts

    rankCoefficients <-
      mapM
        (\x -> fresh >>= \i -> return (RankIdx x, Coeff i))
        [1..nts]

    vecCoefficients <-
      mapM
        (\vec -> fresh >>= \i -> return (VecIdx (V.fromList vec), Coeff i))
        (enum u (nts + 1))

    return $
      ctx { ctxVariables = ctxVariables ++ xs
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
        r' <- augmentCtx bound r ctxVariables
        return (reverse acc, r')
    go ctx@Ctx{..} (y:ys) acc =
      case lookup y ctxVariables of
        Just ty ->
          let ctx' = ctx { ctxVariables = L.delete' y ctxVariables }
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
  case L.deleteAll' xs ctxVariables of
    (y, _) : _ ->
      splitCtx u q [y] >>=
        \case
          ([(y, t)], q') -> return ((y, t), q')
          _              -> throwError $ AssertionFailed "weakenCtx: unexpected splitCtx behavior"
    _ -> throwError $ AssertionFailed "weakenCtx: cannot weaken"

ppCtx :: Ctx -> Text
ppCtx Ctx{..} =
  "variables: "
    <> T.intercalate ", " (map ppVar ctxVariables)
    <> "\n"
    <> "coefficients:\n"
    <> T.intercalate "\n" (map ppCoeff . M.toList $ ctxCoefficients)

ppVar :: (Text, Type) -> Text
ppVar (x, ty) = x <> " : " <> ppTerm' ty

ppCoeff :: (Idx, Coeff) -> Text
ppCoeff (idx, (Coeff i)) = T.pack (show i) <> ": " <> c
  where
    c = case idx of
          RankIdx x  -> "q(" <> T.pack (show x) <> ")"
          -- TODO: fix output (?)
          VecIdx vec -> "q(" <> T.intercalate ", " (map (T.pack . show) . V.toList $ vec) <> ")"

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

coeffs :: Ctx -> [(Idx, Coeff)]
coeffs Ctx{..} = M.toList ctxCoefficients

enumRankCoeffs :: Ctx -> [(Idx, Coeff)]
enumRankCoeffs Ctx{..} = filter (isRankIdx . fst) . M.toList $ ctxCoefficients

returnCtx :: Bound -> Gen Ctx
returnCtx b =
  emptyCtx b >>= \q ->
    augmentCtx b q [("*", tyTree)]

copyCtx :: Ctx -> Gen Ctx
copyCtx q =
  do
    r <- freshCtx
    cs <- forM (M.toList . ctxCoefficients $ q) $
            \(i, _) ->
              Coeff <$> fresh >>= \c ->
                return (i, c)
    return $
      r { ctxVariables    = ctxVariables q
        , ctxCoefficients = M.fromList cs
        }

eqCtx :: Ctx -> Ctx -> Gen ()
eqCtx q r =
  let f = S.fromList . M.keys . ctxCoefficients
      kq = f q
      kr = f r
  in
  if kq /= kr
    then do
      ruleName <- getRuleName
      let g = T.intercalate "," . map (T.pack . show) . S.toList
      let m = "eqCtx (" <> ruleName <> "): contexts differ (Q: " <> g kq <> "; R: " <> g kr <> ")"
      throwError (AssertionFailed m)
    else
      forM_ kq $ \i -> do
        qi <- coeff q i
        ri <- coeff r i
        accumConstr $ [CEq (CAtom qi) (CAtom ri)]

instance Latex Ctx where
  latex Ctx{..} =
    if null ctxVariables
      then "\\varnothing"
      else T.intercalate ", " $ map f ctxVariables
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

getRuleName :: Gen Text
getRuleName =
  gsProofTreeRuleName <$> get >>=
    return . maybe "<unset>" unRuleName

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

-- * Interface

varIdx :: Ctx -> Text -> Gen Idx
varIdx q x =
  case L.elemIndex x . trees $ q of
    Just i  -> return $ RankIdx $ i + 1
    Nothing -> throwError $ AssertionFailed $ "varIdx: tree " <> x <> " not found in context"

-- TODO: cost part, i.e. "+"
vecIdx :: Ctx -> [(Text, Int)] -> Gen Idx
vecIdx q xs =
  do
    when (length xs /= m + 1) $
      throwError $ AssertionFailed $ "vecIdx: subscript with bad length (is " <> T.pack (show . length $ xs) <> ", must be " <> T.pack (show (m + 1))
    xs' <- mapM f xs
    liftIO $ do
      v <- V.thaw $ V.replicate (m + 1) 0
      forM_ xs' $ \(index, value) ->
        MV.write v index value
      VecIdx <$> V.freeze v
  where
    f (x, v)
      | x == costId = return (m, v)
      | otherwise =
          case L.elemIndex x ts of
            Just i  -> return (i, v)
            Nothing -> throwError $ AssertionFailed $ "vecIdx: tree " <> x <> " not found in context"
    ts = trees q
    m = length ts
