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

module PostSummary
  ( component,
  )
where

import Data.Foldable (for_)
import Data.String (IsString (fromString))
import Data.Time (Day)
import Data.Vector (Vector)
import GHC.Records.Compat (HasField)
import Markdown (Markdown)
import qualified Markdown
import qualified Post
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
    (H.header ! A.class_ "bb b--near-white sticky top-0 bg-white") do
      (H.section ! A.class_ "mw7 center lh-copy pa3 flex flex-wrap justify-between") do
        (H.a ! A.href (fromString props.url) ! A.class_ "link hover-dark-green") do
          (H.h1 ! A.class_ "f5 ma0") do
            H.strong do
              H.string props.title
              " "
            (H.div ! A.class_ "dib") do
              "— "
              H.string (Post.formattedDate props.date)
        (H.div ! A.class_ "post-tags") do
          for_ props.tags \tag ->
            (H.span ! A.class_ "ttc") do
              H.string tag

    (H.section ! A.class_ "mw7 center pa3 pt4 block-columns-2-ns markdown lh-copy") do
      Markdown.toHtml props.description

    (H.footer ! A.class_ "mw7 center ph3 pt0 pb4") do
      (H.a ! A.href (fromString props.url) ! A.class_ "link hover-dark-green founders-grotesk") do
        "Read more →"
    H.hr ! A.class_ "ba-0 bt bw3 b--near-white"
