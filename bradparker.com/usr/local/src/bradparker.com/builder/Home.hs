{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Home (render) where

import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import qualified Document
import Post (Post (..))
import qualified PostSummary
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

render :: [Post] -> LBS.ByteString
render = renderHtml . component

component :: [Post] -> Html
component posts =
  let props = Document.Props {title = "Home", url = "/", index = True, thumbnail = Nothing}
   in Document.component props do
        H.article do
          (H.section ! A.class_ "mw7 center pa3") do
            (H.h1 ! A.class_ "f2 ttu tc founders-grotesk-extra-condensed lh-title mt2 mb2") do
              (H.span ! A.class_ "fine-underline") do
                "Hi, I'm"
              H.img ! A.alt "Brad" ! A.src "/assets/images/brad.webp" ! A.class_ "mw6 w-100 center db"
              (H.span ! A.class_ "fine-underline") do
                "Pleased to meet you."
          (H.footer ! A.class_ "bt b--near-white") do
            (H.section ! A.class_ "mw7 center flex flex-wrap lh-copy pa3") do
              (H.a ! A.class_ "link hover-dark-green mr1" ! A.href "https://bne.social/@brad" ! A.rel "me") do
                "bne.social/@brad"
              (H.span ! A.class_ "moon-gray mr1") do
                " / "
              (H.a ! A.class_ "link hover-dark-green" ! A.href "https://github.com/bradparker" ! A.rel "me") do
                "github.com/bradparker"
          H.hr ! A.class_ "ba-0 bt bw3 b--near-white"

        traverse_ PostSummary.component posts
