{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Markdown
  ( Markdown,
    read,
    toHtml,
    toText,
    absoluteUrls,
  )
where

import Data.Either (fromRight)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import System.FilePath (isAbsolute)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.HTML.TagSoup as TagSoup
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Definition (Block (RawBlock), Inline (Image, Link))
import Text.Pandoc.Walk (walk)
import Prelude hiding (read)

newtype Markdown = Markdown {unMarkdown :: Pandoc}
  deriving (Show)

read :: Text -> Markdown
read =
  Markdown
    . fromRight mempty
    . Pandoc.runPure
    . Pandoc.readCommonMark
      Pandoc.def
        { Pandoc.readerExtensions =
            Pandoc.extensionsFromList
              [ Pandoc.Ext_tex_math_dollars,
                Pandoc.Ext_footnotes,
                Pandoc.Ext_gfm_auto_identifiers,
                Pandoc.Ext_definition_lists,
                Pandoc.Ext_attributes
              ]
        }

toHtml :: Markdown -> Html
toHtml =
  either mempty id
    . Pandoc.runPure
    . Pandoc.writeHtml5 Pandoc.def {Pandoc.writerHTMLMathMethod = Pandoc.MathML}
    . unMarkdown

toText :: Markdown -> Text
toText =
  TextLazy.toStrict
    . Blaze.renderHtml
    . either mempty id
    . Pandoc.runPure
    . Pandoc.writeHtml5
      Pandoc.def
        { Pandoc.writerHTMLMathMethod = Pandoc.MathML,
          Pandoc.writerHighlightStyle = Nothing
        }
    . unMarkdown

absoluteUrls :: Text -> Markdown -> Markdown
absoluteUrls base =
  Markdown
    . absoluteUrlsPandoc base
    . unMarkdown

absoluteUrlsPandoc :: Text -> Pandoc -> Pandoc
absoluteUrlsPandoc base pandoc =
  pandoc
    & walk \case
      Image attrs children (url, title) -> Image attrs children (addBaseText url, title)
      Link attrs children (url, title) -> Link attrs children (addBaseText url, title)
      a -> a
    & walk \case
      RawBlock format source -> RawBlock format (Text.pack (addBasesRaw (Text.unpack source)))
      a -> a
  where
    addBasesRaw source = case TagSoup.parseTags source of
      [] -> source
      tags ->
        TagSoup.renderTags
          ( flip map tags \case
              TagSoup.TagOpen "img" attributes ->
                TagSoup.TagOpen
                  "img"
                  ( flip map attributes \case
                      ("src", str) -> ("src", addBase str)
                      attr -> attr
                  )
              TagSoup.TagOpen "audio" attributes ->
                TagSoup.TagOpen
                  "audio"
                  ( flip map attributes \case
                      ("src", str) -> ("src", addBase str)
                      attr -> attr
                  )
              TagSoup.TagOpen "a" attributes ->
                TagSoup.TagOpen
                  "a"
                  ( flip map attributes \case
                      ("href", str) -> ("href", addBase str)
                      attr -> attr
                  )
              tag -> tag
          )

    addBase str =
      if isAbsolute str
        then Text.unpack base <> str
        else str

    addBaseText = Text.pack . addBase . Text.unpack
