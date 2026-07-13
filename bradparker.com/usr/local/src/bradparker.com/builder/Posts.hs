{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Posts (fromFiles, render, component) where

import Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.List (partition, sortOn)
import Data.Maybe (isJust)
import Data.Ord (Down (Down))
import qualified Document
import Post (Post (..))
import qualified Post
import qualified PostSummary
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

fromFiles :: [FilePath] -> Builder ([Post], [Post])
fromFiles files =
  partition (isJust . (.published)) . sortOn (Down . (.date))
    <$> traverse Post.fromFile files

render :: [Post] -> LBS.ByteString
render = renderHtml . component

component :: [Post] -> Html
component posts =
  Document.component (Document.Props "Posts" "/posts" Nothing True) do
    (H.header ! A.class_ "bb b--near-white") do
      (H.section ! A.class_ "mw7 center pa3") do
        (H.h1 ! A.class_ "f2 b founders-grotesk lh-solid ma0 mv3") do
          "Posts"
    traverse_ PostSummary.component posts
