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
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Main
  ( main,
  )
where

import Builder
  ( getConfig,
    build,
    copyFile,
    getDirectoryFiles,
    outputFile,
    toFile,
  )
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import qualified Feed
import qualified Home
import qualified Page
import qualified Post
import qualified Posts
import qualified Tag

main :: IO ()
main = do
  config <- getConfig
  build config do
    traverse_ copyFile =<< getDirectoryFiles ["assets/**/*.*", "static/**/*.*"]

    posts <- sortOn (Down . (.date)) <$> (traverse Post.fromFile =<< getDirectoryFiles ["content/posts/*.md"])

    traverse_ (toFile <$> (.url) <*> Post.render) posts

    toFile "/" (Home.render posts)

    toFile "/posts" (Posts.render posts)

    traverse_ (toFile <$> Tag.url <*> Tag.render) (Tag.fromPosts posts)

    about <- Page.fromFile "content/about.md" "/about"
    toFile about.url (Page.render about)

    resume <- Page.fromFile "content/resume.html" "/resume"
    toFile resume.url (Page.render resume)

    traverse_ (outputFile "rss.xml") (Feed.render posts)
