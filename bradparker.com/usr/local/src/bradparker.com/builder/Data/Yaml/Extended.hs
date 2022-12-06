module Data.Yaml.Extended
  ( module Data.Yaml,
    parse,
  )
where

import Data.ByteString (ByteString)
import Data.Bifunctor (first)
import Data.Yaml

parse :: (Value -> Parser a) -> ByteString -> Either String a
parse parser input = parseEither parser =<< first show (decodeEither' input)
