{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Markdown
  ( Markdown,
    read,
    toHtml,
    toText,
    absoluteUrls,
  )
where

import Control.Lens ((%~), (&), _1, _2)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import System.FilePath (isAbsolute)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Lens (blockInlines, body, _Image, _Link)
import Prelude hiding (read)

newtype Markdown = Markdown {unMarkdown :: Pandoc}
  deriving (Show)

read :: Text -> Markdown
read =
  Markdown
    . fromRight mempty
    . Pandoc.runPure
    . Pandoc.readCommonMark Pandoc.def

toHtml :: Markdown -> Html
toHtml =
  either mempty id
    . Pandoc.runPure
    . Pandoc.writeHtml5 Pandoc.def
    . unMarkdown

toText :: Markdown -> Text
toText =
  TextLazy.toStrict
    . Blaze.renderHtml
    . either mempty id
    . Pandoc.runPure
    . Pandoc.writeHtml5 Pandoc.def {Pandoc.writerHighlightStyle = Nothing}
    . unMarkdown

absoluteUrls :: Text -> Markdown -> Markdown
absoluteUrls base =
  Markdown
    . absoluteUrlsPandoc base
    . unMarkdown

absoluteUrlsPandoc :: Text -> Pandoc -> Pandoc
absoluteUrlsPandoc base pandoc =
  pandoc
    & body . traverse . blockInlines . _Image . _2 . _1 %~ addBase
    & body . traverse . blockInlines . _Link . _2 . _1 %~ addBase
  where
    addBase t =
      if isAbsolute (Text.unpack t)
        then base <> t
        else t
