{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Post
  ( Post (..),
    isPublished,
    component,
    fromFile,
    fromFileToNamespace,
    render,
    formattedDate,
  )
where

import Builder (Builder)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector (Vector)
import Data.Yaml.Extended ((.:), (.:?))
import qualified Data.Yaml.Extended as Yaml
import qualified Document
import GHC.Records (HasField (getField))
import Markdown (Markdown)
import qualified Markdown
import System.FilePath (takeBaseName, takeFileName, (</>))
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Post = Post
  { url :: String,
    created :: Day,
    published :: Maybe Day,
    title :: String,
    tags :: Vector String,
    description :: Markdown,
    thumbnail :: Maybe String,
    content :: Markdown,
    rssGuid :: Maybe String
  }

instance HasField "date" Post Day where
  getField :: Post -> Day
  getField post = fromMaybe post.created post.published

instance HasField "index" Post Bool where
  getField :: Post -> Bool
  getField = isPublished

isPublished :: Post -> Bool
isPublished = isJust . (.published)

data Frontmatter = Frontmatter
  { title :: String,
    tags :: Vector String,
    description :: String,
    thumbnail :: Maybe String,
    published :: Maybe Day,
    rssGuid :: Maybe String
  }

frontmatterParser :: Yaml.Value -> Yaml.Parser Frontmatter
frontmatterParser =
  Yaml.withObject "PageFrontmatter" \o ->
    Frontmatter
      <$> o .: "title"
      <*> o .: "tags"
      <*> o .: "description"
      <*> o .:? "thumbnail"
      <*> o .:? "published"
      <*> o .:? "rss_guid"

slugFromPath :: FilePath -> String
slugFromPath = drop 11 . takeBaseName

dateFromPath :: FilePath -> IO Day
dateFromPath = iso8601ParseM . take 10 . takeFileName

fromFileToNamespace :: String -> FilePath -> Builder Post
fromFileToNamespace namespace path = do
  document <- Document.fromFile frontmatterParser path
  created <- liftIO (dateFromPath path)
  pure
    Post
      { url = namespace </> slugFromPath path,
        created = created,
        title = document.frontmatter.title,
        tags = document.frontmatter.tags,
        description = Markdown.read (Text.pack document.frontmatter.description),
        thumbnail = document.frontmatter.thumbnail,
        content = Markdown.read document.content,
        published = document.frontmatter.published,
        rssGuid = document.frontmatter.rssGuid
      }

fromFile :: FilePath -> Builder Post
fromFile = fromFileToNamespace "posts"

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
    HasField "thumbnail" props (Maybe String),
    HasField "date" props Day,
    HasField "index" props Bool
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
            H.string ("Posted on " <> formattedDate props.date)
          (H.section ! A.class_ "markdown markdown--full") do
            children
