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
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Definition (Inline (Image, Link))
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
              [ Pandoc.Ext_tex_math_dollars
              , Pandoc.Ext_footnotes
              , Pandoc.Ext_gfm_auto_identifiers
              , Pandoc.Ext_definition_lists
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
      Image attrs children (url, title) -> Image attrs children (addBase url, title)
      Link attrs children (url, title) -> Link attrs children (addBase url, title)
      a -> a
  where
    addBase t =
      if isAbsolute (Text.unpack t)
        then base <> t
        else t
