{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Post
  ( Post (..),
    component,
    fromFile,
    render,
    formattedDate,
  )
where

import Builder (Builder)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector (Vector)
import qualified Data.Yaml.Combinators.Extended as Yaml
import qualified Document
import GHC.Records.Compat (HasField)
import Markdown (Markdown)
import qualified Markdown
import System.FilePath (takeBaseName, takeFileName, (</>))
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Post = Post
  { url :: String,
    date :: Day,
    title :: String,
    tags :: Vector String,
    description :: Markdown,
    content :: Markdown,
    rssGuid :: Maybe String
  }

data Frontmatter = Frontmatter
  { title :: String,
    tags :: Vector String,
    description :: String,
    rssGuid :: Maybe String
  }

frontmatterParser :: Yaml.Parser Frontmatter
frontmatterParser =
  Yaml.object $
    Frontmatter
      <$> Yaml.field "title" Yaml.string
      <*> Yaml.field "tags" (Yaml.array Yaml.string)
      <*> Yaml.field "description" Yaml.string
      <*> Yaml.optField "rss_guid" Yaml.string

slugFromPath :: FilePath -> String
slugFromPath = drop 11 . takeBaseName

dateFromPath :: FilePath -> IO Day
dateFromPath = iso8601ParseM . take 10 . takeFileName

fromFile :: FilePath -> Builder Post
fromFile path = do
  document <- Document.fromFile frontmatterParser path
  date <- liftIO (dateFromPath path)
  pure
    Post
      { url = "posts" </> slugFromPath path,
        date = date,
        title = document.frontmatter.title,
        tags = document.frontmatter.tags,
        description = Markdown.read (Text.pack document.frontmatter.description),
        content = Markdown.read document.content,
        rssGuid = document.frontmatter.rssGuid
      }

render :: Post -> ByteString
render post =
  renderHtml $
    component post $
      Markdown.toHtml post.content

formattedDate :: Day -> String
formattedDate = formatTime defaultTimeLocale "%d %B, %Y"

component ::
  forall props.
  ( HasField "url" props String,
    HasField "title" props String,
    HasField "date" props Day
  ) =>
  props ->
  Html ->
  Html
component props children =
  Document.component props do
    H.article do
      (H.section ! A.class_ "mw7 center") do
        (H.section ! A.class_ "measure pa3 lh-copy") do
          (H.h1 ! A.class_ "f2 lh-solid b ma0 mv3") do
            H.string props.title
          (H.p ! A.class_ "mv3") do
            H.small do
              H.string ("Posted on " <> formattedDate props.date)
          (H.section ! A.class_ "markdown markdown--full") do
            children
