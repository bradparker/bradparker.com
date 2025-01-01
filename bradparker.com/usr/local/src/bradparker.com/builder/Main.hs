{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Builder
  ( build,
    copyFile,
    getConfig,
    getDirectoryFiles,
    outputFile,
    toFile,
  )
import Data.Foldable (traverse_)
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

    (posts, drafts) <- Posts.fromFiles =<< getDirectoryFiles ["content/posts/*.md"]

    traverse_ (toFile <$> (.url) <*> Post.render) drafts
    traverse_ (toFile <$> (.url) <*> Post.render) posts

    toFile "/" (Home.render posts)

    toFile "/posts" (Posts.render posts)

    traverse_ (toFile <$> Tag.url <*> Tag.render) (Tag.fromPosts posts)

    about <- Page.fromFile "content/about.md" "/about"
    toFile about.url (Page.render about)

    resume <- Page.fromFile "content/resume.html" "/resume"
    toFile resume.url (Page.render resume)

    notes <- traverse (Post.fromFileToNamespace "notes") =<< getDirectoryFiles ["content/notes/*.md"]
    traverse_ (toFile <$> (.url) <*> Post.render) notes

    traverse_ (outputFile "rss.xml") (Feed.render posts)
