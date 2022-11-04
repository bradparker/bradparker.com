{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Frontmatter (parser) where

import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    endOfLine,
    manyTill,
  )
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml.Extended as Yaml

parser :: (Yaml.Value -> Yaml.Parser a) -> Parser a
parser p = do
  f <- separator *> manyTill anyChar separator
  case Yaml.parse p (Text.encodeUtf8 (Text.pack f)) of
    Left e -> fail (show e)
    Right v -> pure v

separator :: Parser ()
separator = "---" *> endOfLine
