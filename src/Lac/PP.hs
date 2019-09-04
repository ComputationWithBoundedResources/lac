{-# LANGUAGE OverloadedStrings #-}

module Lac.PP where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T

latexVar :: Text -> Text
latexVar x =
  let (p, s) = T.break isDigit x
  in
  if T.all isDigit s
    then escape p <> "_{" <> s <> "}"
    else escape p <> s

escape :: Text -> Text
escape = T.replace "$" "\\$"

tshow :: Show a => a -> Text
tshow = T.pack . show
