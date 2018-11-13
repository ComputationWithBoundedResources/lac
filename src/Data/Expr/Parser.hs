{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Parser where

import           Data.Expr.Types

import           Control.Applicative ((<*))
import           Control.Monad       (void, when)
import           Data.List.NonEmpty
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Parsec
import           Text.Parsec.Helpers (many1')

data CmpOp
  = CmpLt
  | CmpEq
  | CmpGt
  deriving (Show, Eq)

cmpOp :: Stream s m Char => ParsecT s u m CmpOp
cmpOp = op <* spaces
  where
    op = (string "<" *> return CmpLt)
      <|> (string ">" *> return CmpGt)
      <|> (string "=" *> return CmpEq)

identifier :: Stream s m Char => ParsecT s u m String
identifier = do
  let startChar = letter <|> char '_'
  c  <- startChar
  cs <- many (startChar <|> digit)
  ps <- many $ char '\''
  spaces
  return $ c : (cs ++ ps)

ite :: Stream s m Char => ParsecT s u m Expr
ite = do
  keyword "if"
  e1 <- expr
  keyword "then"
  e2 <- expr
  keyword "else"
  e3 <- expr
  return $ Ite e1 e2 e3

match :: Stream s m Char => ParsecT s u m Expr
match =
  do
    keyword "match"
    e <- expr
    keyword "with"
    cs <- many1' case_
    return $ Match e cs
  where
    case_ = do
      pipe
      p <- nil <|> node
      arrow
      e <- expr
      return (p, e)

tree :: Stream s m Char => ParsecT s u m Expr -> ParsecT s u m Literal
tree p = do
  string "{" >> spaces
  e1 <- p
  string "," >> spaces
  e2 <- p
  string "," >> spaces
  e3 <- p
  string "}" >> spaces
  return $ LNode e1 e2 e3

nil :: Stream s m Char => ParsecT s u m Pattern
nil = keyword "nil" *> return PNil

node :: Stream s m Char => ParsecT s u m Pattern
node = do
  LNode (V x) (V y) (V z) <- tree (V <$> var)
  return $ PNode x y z

expr :: Stream s m Char => ParsecT s u m Expr
expr = (controlExpr <|> valueExpr) `chainl1` (op <$> cmpOp)
  where
    op =
      \case
        CmpLt -> (:<)
        CmpEq -> (:==)
        CmpGt -> (:>)

controlExpr :: Stream s m Char => ParsecT s u m Expr
controlExpr = try match <|> ite

valueExpr :: Stream s m Char => ParsecT s u m Expr
valueExpr =
  do
    (x, xs) <- app
    return $
      if null xs
        then x
        else
          let V f = x
          in
          Fun f (mapMaybe fromVar xs)

app :: Stream s m Char => ParsecT s u m (Expr, [Expr])
app =
  do
    (f :| xs) <- many1' p
    return (f, xs)
  where
    p = try (L <$> literal) <|> try (V <$> var)

decl :: Stream s m Char => ParsecT s u m Decl
decl =
  do
    (name, args) <- hd
    string "=" >> spaces
    e <- expr
    string ";" >> spaces
    return $ Decl name args e
  where
    hd =
      app >>=
        \case
          (V f, xs) -> return (f, mapMaybe fromVar xs)
          _         -> parserZero

fromVar :: Expr -> Maybe Text
fromVar (V x) = Just x
fromVar _ = Nothing

keyword :: Stream s m Char => String -> ParsecT s u m ()
keyword s = void $ string s <* spaces

arrow :: Stream s m Char => ParsecT s u m ()
arrow = void $ string "->" >> spaces

pipe :: Stream s m Char => ParsecT s u m ()
pipe = void $ string "|" >> spaces

var :: Stream s m Char => ParsecT s u m Text
var =
  do
    i <- identifier
    when (i `elem` reserved) parserZero
    return (T.pack i)
  where
    reserved = ["if", "then", "else", "match", "with", "true", "false"]
    -- TODO: nil, let, in

-- TODO: `nil` (empty tree)
literal :: Stream s m Char => ParsecT s u m Literal
literal = true <|> false <|> tree expr
  where
    true = keyword "true" *> return (LBool True)
    false = keyword "false" *> return (LBool False)
