{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      hiding (expr)
import           Data.Subst
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Types
import           Lac.TypeInference
import           Latex

import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- * Rules

type Rule a = Subst -> Ctx -> Expr -> Ctx -> Gen a

-- | Dispatch rule
dispatch :: Rule ProofTree
dispatch subst ctx expr ctx' = do
  gen <- case expr of
    Abs _ _   -> throwError $ NotImplemented "abstraction"
    Lit _     -> return genLit
    Cmp _ _ _ -> throwError $ NotImplemented "comparison"
    Ite _ _ _ -> return genIte
    Let _ _ _ -> throwError $ NotImplemented "let"
    App _ _   -> throwError $ NotImplemented "application"
    Match _ _ -> return genMatch
    Var _     -> return genVar
  gen subst ctx expr ctx'

genVar :: Rule ProofTree
-- TODO: compare contexts
genVar subst ctx expr@(Var x) ctxR =
  do
    splitCtx x ctx
    return $ mkConcl ctx expr ctxR `provedBy` [assume $ x <> " \\mbox{ is a variable}"]
genVar subst _ _ _ = throwError $ NotApplicable "variable"

genLit :: Rule ProofTree
genLit subst ctx@Ctx{..} expr@(Lit LNil) ctxR =
  case ctxMembers of
    [] -> return $ mkConcl ctx expr ctxR `provedBy` [assume "TODO"]
    -- apply weakening
    (x, _) : xs -> do
      -- TODO: constraints
      let ctx' = ctx { ctxMembers = xs }
      proof <- dispatch subst ctx' expr ctxR
      return $ mkConcl ctx expr ctxR `provedBy` [proof]
genLit subst _ _ _ = throwError $ NotApplicable "literal"

genMatch :: Rule ProofTree
genMatch subst ctx expr@(Match (Var x) ((PNil, e1) :| [(pNode@(PNode x1 x2 x3), e2)])) ctxR =
  do
    (ty, ctx'@Ctx{..}) <- splitCtx x ctx
    -- TODO: check type
    proof1 <- dispatch subst ctx' e1 ctxR
    -- TODO: use fresh type variable instead of abstract type here?
    ctx'' <- augmentCtx [ (x1, AnTy (tyTree tyNat) ())
                        , (x2, AnTy tyNat          ())
                        , (x3, AnTy (tyTree tyNat) ())
                        ] ctx
    proof2 <- dispatch subst ctx'' e2 ctxR

    e1' <- nameExpr e1
    e2' <- nameExpr e2
    let expr' = Match (Var x) ((PNil, e1') :| [(pNode, e2')])
    return $ mkConcl ctx expr' ctxR `provedBy` [proof1, proof2]
genMatch subst _ _ _ =
  throwError $ NotApplicable "match"

genIte :: Rule ProofTree
genIte subst ctx expr@(Ite (Var x) e1 e2) ctxR =
  do
    (_, ctx') <- splitCtx x ctx
    -- TODO: check type
    proof1 <- dispatch subst ctx' e1 ctxR
    proof2 <- dispatch subst ctx' e2 ctxR

    e1' <- nameExpr e1
    e2' <- nameExpr e2
    return $
      mkConcl ctx (Ite (Var x) e1' e2') ctxR `provedBy` [proof1, proof2]

mkConcl :: Ctx -> Expr -> Ctx -> Text
mkConcl ctxL expr ctxR =
  T.unwords
    [ latex ctxL
    , "\\vdash"
    , latex expr
    , ": "
    , latex ctxR
    ]

writeProof :: FilePath -> Subst -> Ctx -> Expr -> IO ()
writeProof path subst ctx expr =
  let q' = nullCtx "Q'"
      f = dispatch subst ctx expr q'
  in
  runGen f >>=
    \(e, Output{..}) ->
      let eqs = outEqs
      in
      case e of
        Left e -> print e
        Right p ->
          let content = concat [
                  "\\documentclass[12pt,preview]{standalone}"
                , "\\usepackage{amssymb}"
                , "\\usepackage{amsmath}"
                , "\\usepackage{proof}"
                , "\\begin{document}"
                , T.unpack (latex p)
                , "\n\n\n"
                , T.unpack eqAry
                , "\\end{document}"
                ]
              eqAry
                | null eqs = ""
                | otherwise =
                    T.concat [
                        "\\begin{align*}\n"
                      , T.intercalate "\\\\\n" (map (\(a, b) -> a <> " &= " <> b) eqs)
                      , "\\end{align*}\n"
                      ]
          in
          do writeFile path content
             print eqs

nameExpr :: Expr -> Gen Expr
nameExpr expr
  | isSimple expr = return expr
  | otherwise = do
      x <- freshVar "e"
      tell $ outEq x (latex expr)
      return (Var x)

freshVar :: Text -> Gen Text
freshVar prefix = do
  i <- fresh
  return $ prefix <> "_{" <> T.pack (show i) <> "}"
