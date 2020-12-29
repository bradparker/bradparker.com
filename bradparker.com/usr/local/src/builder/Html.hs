{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Html
  ( Html,
    read,
    toText,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as TextLazy
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Pandoc as Pandoc
import Prelude hiding (read)

read :: Text -> Html
read =
  either (const mempty) id
    . Pandoc.runPure
    . Pandoc.writeHtml5 Pandoc.def
    . either (const mempty) id
    . Pandoc.runPure
    . Pandoc.readHtml Pandoc.def

toText :: Html -> Text
toText = TextLazy.toStrict . Blaze.renderHtml
