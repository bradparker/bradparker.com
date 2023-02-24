{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Tag
  ( render,
    component,
    fromPosts,
    url,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Document
import Post (Post(..))
import qualified PostSummary
import System.FilePath ((</>))
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Tag = Tag {name :: String, posts :: [Post]}

url :: Tag -> String
url = ("/tags" </>) . (.name)

fromPosts :: [Post] -> [Tag]
fromPosts = map (uncurry Tag) . Map.assocs . postsByTag
  where
    postsByTag :: [Post] -> Map String [Post]
    postsByTag =
      foldr
        ( \post byTag ->
            foldr (\tag -> Map.insertWith (<>) tag [post]) byTag post.tags
        )
        Map.empty

render :: Tag -> LBS.ByteString
render = renderHtml . component

component :: Tag -> Html
component tag =
  Document.component (Document.Props ("Posts / " <> tag.name) ("/tags" </> tag.name)) do
    (H.header ! A.class_ "bb b--near-white") do
      (H.section ! A.class_ "mw7 center pa3") do
        (H.h1 ! A.class_ "f2 b founders-grotesk lh-solid ma0 mv3") do
          (H.span ! A.class_ "ttc") do
            H.string tag.name
    traverse_ PostSummary.component tag.posts
