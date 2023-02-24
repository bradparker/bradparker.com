{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Posts (render, component) where

import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import qualified Document
import Post (Post(..))
import qualified PostSummary
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

render :: [Post] -> LBS.ByteString
render = renderHtml . component

component :: [Post] -> Html
component posts =
  Document.component (Document.Props "Posts" "/posts") do
    traverse_ PostSummary.component posts
