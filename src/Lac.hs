{-# LANGUAGE RecordWildCards #-}

module Lac where

import           Data.Expr
import           Data.TypeAnn
import           Lac.Eval

import           Data.List    (find)
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Text    (Text)
import qualified Data.Text.IO as T
import           Text.Parsec  (ParseError, parse)

data Prog
  = Prog {
    progDecls :: [Decl]
  , progEnv   :: Map Text Value
  }
  deriving (Eq, Show)

parseProg :: Text -> Either ParseError Prog
parseProg text =
  case parse prog "<source>" text of
    Left e -> Left e
    Right (decls, tyAnns) ->
      let env = M.fromList . map (\(Decl x xs e _) -> (x, Lac.Eval.fromDecl xs e)) $ decls
      in
        Right (Prog decls' env)
      where
        decls' = map (\(Decl x xs e _) -> Decl x xs e (find (\TypeAnn{..} -> taSym == x) tyAnns)) decls

readProg :: FilePath -> IO (Either ParseError Prog)
readProg path = do
  text <- T.readFile path
  return $ parseProg text
