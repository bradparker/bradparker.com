{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Page
  ( Page (..),
    fromFile,
    component,
    render,
  )
where

import Builder (Builder)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Data.Yaml.Extended ((.:), (.:?))
import qualified Document
import GHC.Records (HasField)
import Markdown (Markdown)
import qualified Markdown
import System.FilePath (takeExtension)
import qualified Text.Blaze as H
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Content = Html Text | Markdown Markdown

data Page = Page
  { url :: String,
    title :: String,
    thumbnail :: Maybe String,
    content :: Content,
    index :: Bool
  }

data Frontmatter = Frontmatter
  { title :: String,
    thumbnail :: Maybe String,
    index :: Bool
  }

frontmatterParser :: Yaml.Value -> Yaml.Parser Frontmatter
frontmatterParser =
  Yaml.withObject "PageFrontmatter" \o ->
    Frontmatter
      <$> o .: "title"
      <*> o .:? "thumbnail"
      <*> (fromMaybe True <$> (o .:? "index"))

fromFile :: FilePath -> FilePath -> Builder Page
fromFile path url = do
  document <- Document.fromFile frontmatterParser path
  pure
    Page
      { url = url,
        title = document.frontmatter.title,
        thumbnail = document.frontmatter.thumbnail,
        index = document.frontmatter.index,
        content = case takeExtension path of
          ".md" -> Markdown (Markdown.read document.content)
          _ -> Html document.content
      }

render :: Page -> LBS.ByteString
render page = renderHtml $
  component page $
    case page.content of
      Markdown c -> Markdown.toHtml c
      Html c -> H.preEscapedToMarkup c

component ::
  forall props.
  ( HasField "url" props String,
    HasField "title" props String,
    HasField "thumbnail" props (Maybe String),
    HasField "index" props Bool,
    HasField "content" props Content
  ) =>
  props ->
  Html ->
  Html
component props children =
  Document.component props do
    H.article do
      (H.section ! A.class_ "mw7 center") do
        (H.section ! A.class_ "measure pa3 lh-copy") do
          case props.content of
            Html _ ->
              children
            Markdown _ -> do
              (H.section ! A.class_ "markdown markdown--full") do
                children
