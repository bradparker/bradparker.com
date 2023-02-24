{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Document
  ( component,
    fromFile,
    Props (..),
    Document (..),
  )
where

import Builder (Builder)
import qualified Builder
import Control.Monad.Except (throwError)
import Data.Attoparsec.Text (Parser, parseOnly, takeText)
import qualified Data.ByteString.Lazy as LBS
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import qualified Frontmatter
import GHC.Records (HasField)
import Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Document a = Document
  {frontmatter :: a, content :: Text}

parser :: (Yaml.Value -> Yaml.Parser a) -> Parser (Document a)
parser frontmatter =
  Document
    <$> Frontmatter.parser frontmatter
    <*> takeText

fromFile :: forall a. (Yaml.Value -> Yaml.Parser a) -> FilePath -> Builder (Document a)
fromFile frontmatter path = do
  content <- Text.decodeUtf8 . LBS.toStrict <$> Builder.inputFile path
  case parseOnly (parser frontmatter) content of
    Left e -> throwError (userError (path <> ": " <> e))
    Right document -> pure document

stylesheet :: H.AttributeValue -> Html
stylesheet url =
  H.link
    ! A.rel "stylesheet"
    ! A.href url

me :: H.AttributeValue -> Html
me url =
  H.link ! A.rel "me" ! A.href url

title_ :: String -> Html
title_ page =
  H.title $
    H.string $
      "Brad Parker - Designer + Developer"
        <> if null page
          then ""
          else " / " <> page

navLink :: String -> String -> Html -> Html
navLink currentUrl url =
  H.a
    ! A.href (fromString url)
    ! A.class_ (fromString ("link hover-dark-green" <> if currentUrl == url then " orange" else ""))

data Props = Props {title :: String, url :: String}

component ::
  forall props.
  ( HasField "title" props String,
    HasField "url" props String
  ) =>
  props ->
  Html ->
  Html
component props children =
  (H.docTypeHtml ! A.lang "en") do
    H.head do
      title_ props.title
      H.meta ! A.charset "utf8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      me "https://bne.social/@brad"
      me "https://github.com/bradparker"
      me "https://twitter.com/@parkerbrads"
      H.link ! A.rel "webmention" ! A.href "https://webmention.io/bradparker.com/webmention"
      H.link ! A.rel "shortcut icon" ! A.href "/assets/images/b.svg" ! A.type_ "image/svg+xml"
      stylesheet "/assets/stylesheets/minimal.css"
      stylesheet "/assets/stylesheets/tachyons.min.css"
      stylesheet "/assets/stylesheets/main.css"

    H.body do
      (H.header ! A.class_ "bb b--near-white sticky top-0 bg-white" ! A.role "banner") do
        (H.section ! A.class_ "mw7 center flex flex-wrap justify-between lh-copy pa3") do
          H.section do
            (H.a ! A.href "/" ! A.class_ "link hover-dark-green") do
              (H.h1 ! A.class_ "f5 ma0 b dib") do
                "Brad Parker"
            " — "
            navLink props.url "/tags/design" do
              "Designer"
            " + "
            navLink props.url "/tags/development" do
              "Developer"
          H.nav do
            navLink props.url "/posts" do
              "Posts"
            " "
            (H.a ! A.href "/rss.xml" ! A.class_ "link hover-dark-green moon-gray") do
              "(RSS)"
            (H.span ! A.class_ "moon-gray") do
              " / "
            navLink props.url "/about" do
              "About"

      children

      (H.footer ! A.class_ "bt b--near-white") do
        (H.section ! A.class_ "mw7 center pa3 lh-copy") do
          ( H.a
              ! A.href "http://github.com/bradparker/bradparker.com"
              ! A.class_ "link hover-dark-green underline"
            )
            do
              "Source"
