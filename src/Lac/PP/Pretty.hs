module Lac.PP.Pretty where

import           Data.Text                 (Text)
import qualified Data.Text                 as T

import qualified Text.PrettyPrint.HughesPJ as PP

class Pretty t where
  pretty :: t -> Text
  pretty = T.pack . PP.renderStyle s . doc
    where
      s = PP.Style PP.PageMode 200 0

  doc :: t -> PP.Doc
