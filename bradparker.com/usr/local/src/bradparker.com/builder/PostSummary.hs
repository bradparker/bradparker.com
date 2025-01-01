{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module PostSummary
  ( component,
  )
where

import Data.Foldable (for_)
import Data.String (IsString (fromString))
import Data.Time (Day)
import Data.Vector (Vector)
import GHC.Records (HasField)
import Markdown (Markdown)
import qualified Markdown
import qualified Post
import System.FilePath ((</>))
import Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

component ::
  forall props.
  ( HasField "url" props String,
    HasField "title" props String,
    HasField "date" props Day,
    HasField "tags" props (Vector String),
    HasField "description" props Markdown
  ) =>
  props ->
  Html
component props =
  H.article do
    (H.header ! A.class_ "bb b--near-white sticky top-0 bg-white flex") do
      (H.section ! A.class_ "w-100 mw7 center lh-copy pa3 flex flex-wrap items-center g2") do
        (H.a ! A.href (fromString ("/" </> props.url)) ! A.class_ "link hover-dark-green mr-auto") do
          (H.h1 ! A.class_ "f4 ma0") do
            H.strong do
              H.string props.title
              " "
        (H.div ! A.class_ "flex flex-wrap items-center shrink-0 g2") do
          H.div do
            H.string (Post.formattedDate props.date)
          (H.div ! A.class_ "flex g2") do
            for_ props.tags \tag ->
              (H.a ! A.class_ "ttc hover-dark-green" ! A.href (fromString ("/tags" </> tag))) do
                H.string tag

    (H.section ! A.class_ "mw7 center pa3 pt4 block-columns-2-ns markdown lh-copy") do
      Markdown.toHtml props.description

    (H.footer ! A.class_ "mw7 center ph3 pt0 pb4") do
      (H.a ! A.href (fromString ("/" </> props.url)) ! A.class_ "link hover-dark-green founders-grotesk") do
        "Read more →"
    H.hr ! A.class_ "ba-0 bt bw3 b--near-white"
