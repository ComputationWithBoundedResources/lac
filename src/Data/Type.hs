{-# LANGUAGE OverloadedStrings #-}

module Data.Type where

import           Data.Term
import           Data.Term.Pretty           (convertTerm)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Lac.PP                     (latexVar)

import           Control.Monad.State.Strict

type Env = [(T String Text, Type)]

type Type = T String Int

latexType :: Type -> Text
latexType = go False . convertTerm
  where
    go _ (V x) = latexVar x
    go l (F "->" [t, u]) = brace l inner
      where inner = go True t <> " \\rightarrow " <> go False u
    go _ (F f []) = "\\mathsf{" <> f <> "}"
    go _ (F f ts) = "\\mathsf{" <> f <> "}" <> "(" <> (T.intercalate ", " . map (go False) $ ts) <> ")"

    brace False x = x
    brace True  x = "(" <> x <> ")"

tyBool :: Type
tyBool = F "Bool" []

tyNat :: Type
tyNat = F "Nat" []

tyTree :: Type
tyTree = F "Tree" [tyNat]

tyFun :: Type -> Type -> Type
tyFun a b = F "->" [a, b]

tyHole :: Type
tyHole = F "_" []

isTyTree :: Type -> Bool
isTyTree = (== tyTree)

isTyNat :: Type -> Bool
isTyNat = (== tyNat)

-- TODO: do not confuse "base type" (i.e. Nat) with "ground" type, i.e.
-- a type that does not contain type variables
isBaseType :: Type -> Bool
isBaseType (V _)    = False
isBaseType (F _ xs) = all isBaseType xs

fromTerm :: T String String -> Type
fromTerm t = fst . runState (fromTerm' t) $ mempty

fromTerm' :: T String String -> State [(String, Int)] Type
fromTerm' = go
  where
    go :: T String String -> State [(String, Int)] Type
    go (V x) = do
      m <- get
      case lookup x m of
        Just x' -> return $ V x'
        Nothing -> do
          let i = case map snd m of
                    [] -> 0
                    xs -> maximum xs + 1
          put $ (x, i) : m
          return $ V i
    go (F f ts) = do
      ts' <- mapM go ts
      return $ F f ts'
