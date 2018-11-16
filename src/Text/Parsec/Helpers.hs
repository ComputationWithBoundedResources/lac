{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Helpers where

import           Data.List.NonEmpty (NonEmpty (..))

import           Text.Parsec

many1' :: ParsecT s u m a -> ParsecT s u m (NonEmpty a)
many1' p = do
  hd <- p
  tl <- many p
  return (hd :| tl)

triple :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m c -> ParsecT s u m (a, b, c)
triple a b c = do
  char '(' >> spaces
  x <- a <* spaces
  char ',' >> spaces
  y <- b <* spaces
  char ',' >> spaces
  z <- c <* spaces
  char ')' >> spaces
  return (x, y, z)

pair :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
pair a b = do
  char '(' >> spaces
  x <- a <* spaces
  char ',' >> spaces
  y <- b <* spaces
  char ')' >> spaces
  return (x, y)
