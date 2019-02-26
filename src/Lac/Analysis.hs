{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      hiding (expr)
import           Data.Term.Pretty
import           Lac.Analysis.Types
import           Lac.TypeInference

import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Proof tree

-- | An intermediate representation of a proof tree that can later be converted to LaTex code.
data ProofTree
  = ProofTree Text [ProofTree]
  deriving (Eq, Show)

class Latex a where
  latex :: a -> Text

instance Latex ProofTree where
  latex (ProofTree concl premises)
    | null premises = concl
    | otherwise     = "\\infer{" <> concl <> "}{" <> T.intercalate " & " (map latex premises) <> "}"

concl `provedBy` ts = ProofTree concl ts

assume s = ProofTree s []

-- * Rules

type Rule a = Ctx -> Expr -> Ctx -> Gen a

-- | Dispatch rule
dispatch :: Rule ProofTree
dispatch ctx expr ctx' = do
  gen <- case expr of
    Abs _ _   -> throwError $ NotImplemented "abstraction"
    Lit _     -> return genLit
    Cmp _ _ _ -> throwError $ NotImplemented "comparison"
    Ite _ _ _ -> return genIte
    Let _ _ _ -> throwError $ NotImplemented "let"
    App _ _   -> throwError $ NotImplemented "application"
    Match _ _ -> return genMatch
    Var _     -> return genVar
  gen ctx expr ctx'

genVar :: Rule ProofTree
-- TODO: compare contexts
genVar ctx expr@(Var x) ctxR =
  do
    splitCtx x ctx
    return $ mkConcl ctx expr ctxR `provedBy` [assume $ "`" <> x <> "` is a variable"]
genVar _ _ _ = throwError $ NotApplicable "variable"

genLit :: Rule ProofTree
-- TODO: apply weakening
genLit ctx@Ctx{..} expr@(Lit LNil) ctxR =
  case ctxMembers of
    [] -> return $ mkConcl ctx expr ctxR `provedBy` [assume "TODO"]
    -- apply weakening
    (x, _) : xs -> do
      -- TODO: constraints
      let ctx' = ctx { ctxMembers = xs }
      proof <- dispatch ctx' expr ctxR
      return $ mkConcl ctx expr ctxR `provedBy` [proof]
genLit _ _ _ = throwError $ NotApplicable "literal"

genMatch :: Rule ProofTree
genMatch ctx expr@(Match (Var x) ((PNil, e1) :| [(PNode x1 x2 x3, e2)])) ctxR =
  do
    (ty, ctx'@Ctx{..}) <- splitCtx x ctx
    -- TODO: check type
    liftIO $ print ty
    proof1 <- dispatch ctx' e1 ctxR
    -- TODO: use fresh type variable instead of abstract type here?
    ctx'' <- augmentCtx [ (x1, AnTy (tyTree tyAbs) ())
                        , (x2, AnTy tyAbs          ())
                        , (x3, AnTy (tyTree tyAbs) ())
                        ] ctx
    proof2 <- dispatch ctx'' e2 ctxR
    return $ mkConcl ctx expr ctxR `provedBy` [proof1, proof2]
genMatch _ _ _ =
  throwError $ NotApplicable "match"

genIte :: Rule ProofTree
genIte ctx expr@(Ite (Var x) e1 e2) ctxR =
  do
    (_, ctx') <- splitCtx x ctx
    -- TODO: check type
    proof1 <- dispatch ctx' e1 ctxR
    proof2 <- dispatch ctx' e2 ctxR
    return $
      mkConcl ctx expr ctxR `provedBy` [proof1, proof2]

mkConcl :: Ctx -> Expr -> Ctx -> Text
mkConcl ctxL expr ctxR =
  T.unwords
    [ ppCtx ctxL
    , "\\vdash"
    , pretty expr
    , ": "
    , ppCtx ctxR
    ]

ppCtx :: Ctx -> Text
ppCtx Ctx{..} =
  if null ctxMembers
    then "\\varnothing"
    else T.intercalate ", " $ map f ctxMembers
  where
    f (x, ty) = x <> ": " <> ppAnTy ty

    -- TODO: format type annotation
    ppAnTy :: AnTy -> Text
    ppAnTy (AnTy ty _) = ppTerm' ty

writeProof :: FilePath -> Ctx -> Expr -> IO ()
writeProof path ctx expr =
  let q' = nullCtx "Q'"
      f = dispatch ctx expr q'
  in
  runGen f >>=
    \(e, cs) ->
      case e of
        Left e -> print e
        Right p ->
          let content = concat [
                  "\\documentclass[a4paper,10pt]{article}"
                , "\\usepackage{amssymb}"
                , "\\usepackage{proof}"
                , "\\usepackage{lscape}"
                , "\\begin{document}"
                , "\\begin{landscape}"
                , T.unpack (latex p)
                , "\\end{landscape}"
                , "\\end{document}"
                ]
          in
          writeFile path content
