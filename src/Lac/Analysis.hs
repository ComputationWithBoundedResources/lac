{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lac.Analysis where

import           Control.Monad.State.Strict.Ext
import           Data.Expr                      hiding (expr)
import           Data.Term.Pretty
import           Lac.Analysis.ProofTree
import           Lac.Analysis.Types
import           Lac.TypeInference
import           Latex

import           Data.List.NonEmpty             (NonEmpty ((:|)))
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T

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
    return $ mkConcl ctx expr ctxR `provedBy` [assume $ x <> " \\mbox{ is a variable}"]
genVar _ _ _ = throwError $ NotApplicable "variable"

genLit :: Rule ProofTree
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
genMatch ctx expr@(Match (Var x) ((PNil, e1) :| [(pNode@(PNode x1 x2 x3), e2)])) ctxR =
  do
    (ty, ctx'@Ctx{..}) <- splitCtx x ctx
    -- TODO: check type
    proof1 <- dispatch ctx' e1 ctxR
    -- TODO: use fresh type variable instead of abstract type here?
    ctx'' <- augmentCtx [ (x1, AnTy (tyTree tyAbs) ())
                        , (x2, AnTy tyAbs          ())
                        , (x3, AnTy (tyTree tyAbs) ())
                        ] ctx
    proof2 <- dispatch ctx'' e2 ctxR

    e1' <- nameExpr e1
    e2' <- nameExpr e2
    let expr' = Match (Var x) ((PNil, e1') :| [(pNode, e2')])
    return $ mkConcl ctx expr' ctxR `provedBy` [proof1, proof2]
genMatch _ _ _ =
  throwError $ NotApplicable "match"

genIte :: Rule ProofTree
genIte ctx expr@(Ite (Var x) e1 e2) ctxR =
  do
    (_, ctx') <- splitCtx x ctx
    -- TODO: check type
    proof1 <- dispatch ctx' e1 ctxR
    proof2 <- dispatch ctx' e2 ctxR

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

writeProof :: FilePath -> Ctx -> Expr -> IO ()
writeProof path ctx expr =
  let q' = nullCtx "Q'"
      f = dispatch ctx expr q'
  in
  runGen f >>=
    \(e, cs) ->
      let eqs = mapMaybe (\case OutEq a b -> Just (a, b); _ -> Nothing) cs
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
  | isSimpleExpr expr = return expr
  | otherwise = do
      x <- freshVar "e"
      tell $ [OutEq x (latex expr)]
      return (Var x)

isSimpleExpr :: Expr -> Bool
isSimpleExpr = (<= 1) . depth

depth :: Expr -> Int
depth (Var _) = 0
depth (Lit l) =
  case l of
    LNil -> 0
    LNode e1 e2 e3 -> 1 + maximum [depth e1, depth e2, depth e3]

    LBool _ -> 0

    LNat _ -> 0
depth (Cmp _ e1 e2) = 1 + maximum [depth e1, depth e2]
depth (Ite x e1 e2) = 2 + maximum [depth x, depth e1, depth e2]
depth (Let _ e1 e2) = 1 + maximum [depth e1, depth e2]
depth (App e1 e2) = 1 + maximum [depth e1, depth e2]
depth (Match t cs) = 2 + depth t + maximum (map (\(_, e) -> depth e) . NE.toList $ cs)
depth (Abs _ e) = 1 + depth e

freshVar :: Text -> Gen Text
freshVar prefix = do
  i <- fresh
  return $ prefix <> "_{" <> T.pack (show i) <> "}"
