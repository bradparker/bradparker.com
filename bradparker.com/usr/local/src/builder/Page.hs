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

module Page
  ( Page (..),
    fromFile,
    component,
    render,
  )
where

import Builder (Builder)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Yaml.Combinators.Extended as Yaml
import qualified Document
import GHC.Records.Compat (HasField)
import Markdown (Markdown)
import qualified Markdown
import System.FilePath (takeExtension)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Html

data Content = Html Html | Markdown Markdown

data Page = Page
  { url :: String,
    title :: String,
    content :: Content
  }

fromFile :: FilePath -> FilePath -> Builder Page
fromFile path url = do
  document <- Document.fromFile (Yaml.object (Yaml.field "title" Yaml.string)) path
  pure
    Page
      { url = url,
        title = document.frontmatter,
        content = case takeExtension path of
          ".md" -> Markdown (Markdown.read document.content)
          _ -> Html (Html.read document.content)
      }

render :: Page -> LBS.ByteString
render page = renderHtml $
  component page $
    case Page.content page of
      Markdown c -> Markdown.toHtml c
      Html c -> c

component ::
  forall props.
  ( HasField "url" props String,
    HasField "title" props String
  ) =>
  props ->
  Html ->
  Html
component props children =
  Document.component props do
    H.article do
      (H.section ! A.class_ "mw7 center") do
        (H.section ! A.class_ "measure pa3 lh-copy") do
          (H.section ! A.class_ "markdown markdown--full") do
            children
