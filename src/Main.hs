{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Applicative (empty)
import Data.Maybe (fromMaybe)
import Hakyll
import System.FilePath ((<.>), (</>))

myDefaultContext :: Context String
myDefaultContext =
  currentPageField
    <> defaultContext

config :: Configuration
config = defaultConfiguration
    { previewPort = 8080
    }

main :: IO ()
main = hakyllWith config $ do
  match "assets/images/*" $ do
    route idRoute
    compile copyFileCompiler

  match ("assets/stylesheets/*" .||. "assets/stylesheets/**/*") $ do
    route idRoute
    compile compressCssCompiler

  tags <- buildTags "content/posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx =
            listField "posts" (postCtxWithTags tags) (return posts)
              <> constField "tag" tag
              <> myDefaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html"     ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "content/posts/*" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
      >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
      >>= relativizeUrls

  create ["content/posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "content/posts/*"
      let postsCtx =
            listField "posts" (postCtxWithTags tags) (return posts)
              <> myDefaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html"   postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls

  match "content/about.md" $ do
    route $ setExtension "html"
    compile
      $   pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html"    (postCtxWithTags tags)
      >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)

  match "content/index.html" $ do
    route $ customRoute $ const "index.html"
    compile $ do
      posts <- recentFirst =<< loadAll "content/posts/*"
      let indexCtx =
            listField "posts" (postCtxWithTags tags) (return posts)
              <> myDefaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

markdownMetadataField :: String -> Context String
markdownMetadataField name = field name $ \item -> do
  let identifier = itemIdentifier item
  val <- fromMaybe "" <$> getMetadataField identifier name
  itemBody <$> renderPandoc
    (Item (fromFilePath (toFilePath identifier </> name <.> "markdown")) val)

currentPageField :: Context String
currentPageField = Context $ \key _ item -> do
  pageUrl <- maybe empty toUrl <$> getRoute (itemIdentifier item)
  if key == sanitize pageUrl then pure (StringField key) else empty
    where
      disallowed c = c == '.' || c == '/'
      replace c = if disallowed c then '-' else c
      sanitize = map replace . dropWhile disallowed

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> markdownMetadataField "summary"
    <> myDefaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx
