module Data.Yaml.Combinators.Extended
  ( module Data.Yaml.Combinators,
    string,
  )
where

import qualified Data.Text as Text
import Data.Yaml.Combinators hiding (string)
import qualified Data.Yaml.Combinators as Yaml

string :: Yaml.Parser String
string = Text.unpack <$> Yaml.string
