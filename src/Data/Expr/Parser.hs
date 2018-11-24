{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Parser where

import           Data.Expr.Types

import           Control.Applicative ((<*))
import           Control.Monad       (void, when)
import           Data.Function.Ext   (uncurry3)
import           Data.List.NonEmpty
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Parsec
import           Text.Parsec.Helpers (many1', parens)

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

let_ :: Stream s m Char => ParsecT s u m Expr
let_ = do
  keyword "let"
  x <- var
  keyword "="
  e1 <- expr
  keyword "in"
  e2 <- expr
  return $ Let x e1 e2

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
      p <- tree var PNil (uncurry3 PNode)
      arrow
      e <- expr
      return (p, e)

tree :: Stream s m Char => ParsecT s u m a -> b -> ((a, a, a) -> b) -> ParsecT s u m b
tree p x g = (nil >> return x) <|> (g <$> node p)

node :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (a, a, a)
node p = do
  string "{" >> spaces
  e1 <- p
  string "," >> spaces
  e2 <- p
  string "," >> spaces
  e3 <- p
  string "}" >> spaces
  return (e1, e2, e3)

nil :: Stream s m Char => ParsecT s u m ()
nil = void $ keyword "nil"

expr :: Stream s m Char => ParsecT s u m Expr
expr = (controlExpr <|> valueExpr) `chainl1` (op <$> cmpOp)
  where
    op =
      \case
        CmpLt -> (:<)
        CmpEq -> (:==)
        CmpGt -> (:>)

controlExpr :: Stream s m Char => ParsecT s u m Expr
controlExpr = try let_ <|> try match <|> try ite

valueExpr :: Stream s m Char => ParsecT s u m Expr
valueExpr =
  do
    (x, xs) <- app
    return $
      if null xs
        then x
        else foldl1 App (x:xs)

app :: Stream s m Char => ParsecT s u m (Expr, [Expr])
app =
  do
    (f :| xs) <- many1' p
    return (f, xs)
  where
    p = parens expr
        <|> (L <$> nat)
        <|> try (L <$> literal)
        <|> try (Var <$> var)

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
          (Var f, xs) -> return (f, mapMaybe fromVar xs)
          _         -> parserZero

fromVar :: Expr -> Maybe Text
fromVar (Var x) = Just x
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
    reserved = ["if", "then", "else", "match", "with", "true", "false", "nil", "let", "in"]

literal :: Stream s m Char => ParsecT s u m Literal
literal = nat <|> true <|> false <|> tree expr LNil (uncurry3 LNode)
  where
    true = keyword "true" *> return (LBool True)
    false = keyword "false" *> return (LBool False)

nat :: Stream s m Char => ParsecT s u m Literal
nat = (LNat . read) <$> (many1 digit <* spaces)
