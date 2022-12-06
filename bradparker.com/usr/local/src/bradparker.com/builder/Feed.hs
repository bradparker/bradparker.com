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

module Feed (render) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as Text
import Data.Time (Day, UTCTime (UTCTime))
import Data.Time.Format (defaultTimeLocale, formatTime, rfc822DateFormat)
import qualified Markdown
import Post (Post)
import System.FilePath ((</>))
import Text.RSS.Export (textRSS)
import Text.RSS.Syntax (RSS (..), RSSChannel (..), RSSGuid (..), RSSItem (..))

render :: [Post] -> Maybe LBS.ByteString
render = fmap Text.encodeUtf8 . textRSS . feed

formatDate :: Day -> Text
formatDate = Text.pack . formatTime defaultTimeLocale rfc822DateFormat . (`UTCTime` 0)

feed :: [Post] -> RSS
feed posts =
  RSS
    { rssVersion = "2.0",
      rssAttrs = [],
      rssChannel = channel posts,
      rssOther = []
    }

channel :: [Post] -> RSSChannel
channel posts =
  RSSChannel
    { rssTitle = "Brad Parker — Designer + Developer",
      rssLink = "https://bradparker.com",
      rssDescription = "I've been making software professionally for about nine years and I really love it. I believe software has this empowering potential, I believe everyone should be able to understand it if they want to. This means I prefer open and accessible tools and standards wherever possible. It also means I try to learn in the open, sharing what I learn as I learn it.",
      rssItems = map item posts,
      rssLanguage = Nothing,
      rssCopyright = Nothing,
      rssEditor = Nothing,
      rssWebMaster = Nothing,
      rssPubDate = Nothing,
      rssLastUpdate = formatDate . (.date) <$> listToMaybe posts,
      rssCategories = [],
      rssGenerator = Nothing,
      rssDocs = Nothing,
      rssCloud = Nothing,
      rssTTL = Nothing,
      rssImage = Nothing,
      rssRating = Nothing,
      rssTextInput = Nothing,
      rssSkipHours = Nothing,
      rssSkipDays = Nothing,
      rssChannelOther = []
    }

item :: Post -> RSSItem
item post =
  let url = "https://bradparker.com" </> post.url
   in RSSItem
        { rssItemTitle = Just (Text.pack post.title),
          rssItemLink = Just (Text.pack url),
          rssItemDescription = Just (Markdown.toText (Markdown.absoluteUrls "https://bradparker.com" post.content)),
          rssItemContent = Just (Markdown.toText (Markdown.absoluteUrls "https://bradparker.com" post.content)),
          rssItemAuthor = Just "Brad Parker",
          rssItemCategories = [],
          rssItemComments = Nothing,
          rssItemEnclosure = Nothing,
          rssItemGuid = RSSGuid Nothing [] . Text.pack <$> (post.rssGuid <|> Just url),
          rssItemPubDate = Just (formatDate post.date),
          rssItemSource = Nothing,
          rssItemAttrs = [],
          rssItemOther = []
        }
