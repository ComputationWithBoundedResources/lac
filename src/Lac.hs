module Lac where

import           Data.Expr
import           Lac.Eval

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
    Right decls ->
      let env = M.fromList . map (\(Decl x xs e) -> (x, Lac.Eval.fromDecl xs e)) $ decls
      in
      Right (Prog decls env)

readProg :: FilePath -> IO (Either ParseError Prog)
readProg path = do
  text <- T.readFile path
  return $ parseProg text
