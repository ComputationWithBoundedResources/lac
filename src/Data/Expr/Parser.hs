{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Expr.Parser where

import           Data.Expr.Types     hiding (var)
import qualified Data.Term           as Term
import           Data.Type
import           Data.TypeAnn

import           Control.Applicative ((<*))
import           Control.Arrow       (first)
import           Control.Monad       (void, when)
import qualified Data.Char           as Char
import           Data.Either         (partitionEithers)
import           Data.Function.Ext   (uncurry3)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Maybe          (fromMaybe)
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Parsec         hiding (spaces)
import           Text.Parsec.Helpers (many1', parens)

prog :: Stream s m Char => ParsecT s u m ([Decl] , [TypeSig])
prog = do
  parts <- many (try (Right <$> typeSig) <|> (Left <$> decl))
  eof
  return (partitionEithers parts)

sEq :: String
sEq = "="

sCmpEq :: String
sCmpEq = "=="

spaces :: Stream s m Char => ParsecT s u m String
spaces = many (oneOf " \t\n\r")

spaces' :: Stream s m Char => ParsecT s u m ()
spaces' = spaces >> optional (try comment >> spaces')

comment :: Stream s m Char => ParsecT s u m String
comment =
  do
    a <- string "(*"
    b <- concat <$> many (try comment <|> fmap toString other)
    c <- string "*)"
    return $ a <> b <> c
  where
    toString x = [x]

    other =
      try (noneOf "(*")
      <|> try (char '*' >> notFollowedBy (char ')') >> return '*')
      <|> try (char '(' >> notFollowedBy (char '*') >> return '(')

eq :: Stream s m Char => ParsecT s u m String
eq = do
  void $ string sEq
  notFollowedBy (string sEq)
  spaces'
  return sEq

cmpEq :: Stream s m Char => ParsecT s u m String
cmpEq = string sCmpEq >> spaces' >> return sCmpEq

cmpOp :: Stream s m Char => ParsecT s u m CmpOp
cmpOp = op <* spaces'
  where
    op = try (string "<=" *> return CmpLe)
      <|> (string "<" *> return CmpLt)
      <|> (string ">=" *> return CmpGe)
      <|> (string ">" *> return CmpGt)
      <|> (cmpEq *> return CmpEq)

identifier :: Stream s m Char => ParsecT s u m String
identifier = do
  let startChar = letter <|> char '_'
  c  <- startChar
  cs <- many (startChar <|> digit)
  ps <- many $ char '\''
  spaces'
  return $ c : (cs ++ ps)

let_ :: Stream s m Char => ParsecT s u m Expr
let_ = do
  keyword "let"
  x <- var
  void eq
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
  string "(" >> spaces'
  e1 <- p
  string "," >> spaces'
  e2 <- p
  string "," >> spaces'
  e3 <- p
  string ")" >> spaces'
  return (e1, e2, e3)

nil :: Stream s m Char => ParsecT s u m ()
nil = void $ keyword "nil"

expr :: Stream s m Char => ParsecT s u m Expr
expr = (controlExpr <|> valueExpr) `chainl1` (op <$> cmpOp)
  where
    op =
      \case
        CmpLt -> (:<)
        CmpLe -> (:<=)
        CmpEq -> (:==)
        CmpGt -> (:>)
        CmpGe -> (:>=)

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
    p = try (parens expr)
        <|> ((Lit . LNat) <$> nat)
        <|> try (Lit <$> literal)
        <|> try (Var <$> var)

decl :: Stream s m Char => ParsecT s u m Decl
decl =
  do
    (name, args) <- hd
    void eq
    e <- expr
    string ";" >> spaces'
    return $ Decl name args e Nothing
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
keyword s = void $ string s <* spaces'

arrow :: Stream s m Char => ParsecT s u m ()
arrow = void $ string "->" >> spaces'

pipe :: Stream s m Char => ParsecT s u m ()
pipe = void $ string "|" >> spaces'

var :: Stream s m Char => ParsecT s u m Text
var =
  do
    i <- identifier
    when (i `elem` reserved) parserZero
    return (T.pack i)
  where
    reserved = ["if", "then", "else", "match", "with", "true", "false", "nil", "let", "in"]

literal :: Stream s m Char => ParsecT s u m Literal
literal = (LNat <$> nat) <|> true <|> false <|> tree expr LNil (uncurry3 LNode)
  where
    true = keyword "true" *> return (LBool True)
    false = keyword "false" *> return (LBool False)

nat :: Stream s m Char => ParsecT s u m Int
nat = read <$> (many1 digit <* spaces')

typeSig :: Stream s m Char => ParsecT s u m TypeSig
typeSig =
  do
    f <- T.pack <$> identifier
    spaces'
    void $ char ':'
    spaces'
    ty <- toType <$> term `sepBy1` arrow
    ma <- fromMaybe mempty <$> optionMaybe ann
    void $ char ';'
    spaces'
    return $ TypeSig f ty ma
  where
    toType :: [Term.T String String] -> Type
    toType = fromTerm . foldr1 fun

    fun x y = Term.F "->" [x, y]

term :: Stream s m Char => ParsecT s u m (Term.T String String)
term = do
  f@(c:_) <- identifier
  if Char.isUpper c
    then do
      -- TODO: reuse application parser
      mxs <- optionMaybe (parens (term `sepBy1` (char ',' >> spaces')))
      let xs = fromMaybe [] mxs
      return $ Term.F f xs
    else
      return $ Term.V f

ann :: Stream s m Char => ParsecT s u m TypeAnn
ann =
  do
    void $ char '{'
    spaces'
    (v, r) <- partitionEithers <$> coeffs
    void $ char '}'
    spaces'
    return $ TypeAnn (map (first T.pack) r) v
  where
    coeffs =
      ((Left <$> vec) <|> (Right <$> rank)) `sepBy` (char ',' >> spaces')
    rank = do
      x <- identifier
      spaces'
      void $ char '='
      spaces'
      v <- nat
      spaces'
      return (x, v)
    vec = do
      void $ char '('
      ss <- nat `sepBy1` (char ',' >> spaces')
      spaces'
      void $ char ')'
      spaces'
      void $ char '='
      spaces'
      v <- nat
      spaces'
      return (ss, v)
