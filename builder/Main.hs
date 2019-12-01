{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
    )
where

import Control.Applicative (empty)
import Data.Maybe (fromMaybe)
import Hakyll
import Hakyll.Web.Html (isExternal, withUrls)
import System.Environment (lookupEnv)
import System.FilePath ((<.>), (</>))

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "Brad Parker &#8212; Designer + Developer",
    feedDescription = "I've been making software professionally for about seven years and I really love it. I believe software has this empowering potential, I believe everyone should be able to understand it if they want to. This means I prefer open and accessible tools and standards wherever possible. It also means I try to learn in the open, sharing what I learn as I learn it.",
    feedAuthorName = "Brad Parker",
    feedAuthorEmail = "hi@bradparker.com",
    feedRoot = "https://bradparker.com"
    }

baseContext :: Context String
baseContext =
  currentPageField
    <> defaultContext

config :: String -> Configuration
config sourceDir =
  defaultConfiguration
    { previewPort = 8080,
      providerDirectory = sourceDir
      }

main :: IO ()
main = do
  sourceDir <- fromMaybe "." <$> lookupEnv "SOURCE_DIR"
  hakyllWith (config sourceDir) $ do
    match ("assets/images/*" .||. "static/*") $ do
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
              listField "posts" (postCtx tags "") (return posts)
                <> constField "tag" tag
                <> baseContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
    match "content/posts/*" $ do
      route $ setExtension "html"
      compile
        $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" (postCtx tags "")
        >>= loadAndApplyTemplate "templates/default.html" (postCtx tags "")
        >>= relativizeUrls
    create ["content/posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "content/posts/*"
        let postsCtx = listField "posts" (postCtx tags "") (return posts) <> baseContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/posts.html" postsCtx
          >>= loadAndApplyTemplate "templates/default.html" postsCtx
          >>= relativizeUrls
    feed "atom.xml" renderAtom (postCtx tags (feedRoot feedConfiguration))
    feed "rss.xml" renderRss (postCtx tags (feedRoot feedConfiguration))
    match "content/about.md" $ do
      route $ setExtension "html"
      compile
        $ pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" (postCtx tags "")
        >>= loadAndApplyTemplate "templates/default.html" (postCtx tags "")
    match "content/resume.html" $ do
      route idRoute
      compile
        $ getResourceBody
        >>= applyAsTemplate (postCtx tags "")
        >>= loadAndApplyTemplate "templates/default.html" (postCtx tags "")
        >>= relativizeUrls
    match "content/index.html" $ do
      route $ customRoute $ const "index.html"
      compile $ do
        posts <- recentFirst =<< loadAll "content/posts/*"
        let indexCtx =
              listField "posts" (postCtx tags "") (return posts)
                <> baseContext
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

feed
  :: Identifier
  -> ( FeedConfiguration
       -> Context String
       -> [Item String]
       -> Compiler (Item String)
       )
  -> Context String
  -> Rules ()
feed identifier renderer ctx =
  create [identifier] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< loadAll "content/posts/*"
      renderer feedConfiguration ctx posts

markdownMetadataField :: String -> String -> Context String
markdownMetadataField root name = field name $ \item -> do
  let identifier = itemIdentifier item
  markdownField <- fromMaybe "" <$> getMetadataField identifier name
  let tempIdentifier = fromFilePath (toFilePath identifier </> name <.> "markdown")
  let tempItem = Item tempIdentifier markdownField
  itemBody <$> (renderPandoc tempItem >>= absoluteUrls root)

absoluteUrls :: String -> Item String -> Compiler (Item String)
absoluteUrls root = pure . fmap (withUrls absolute)
  where
    absolute p = if isExternal p then p else root ++ p

currentPageField :: Context String
currentPageField = Context $ \key _ item -> do
  pageUrl <- maybe empty toUrl <$> getRoute (itemIdentifier item)
  if key == sanitize pageUrl then pure (StringField key) else empty
  where
    disallowed c = c == '.' || c == '/'
    replace c = if disallowed c then '-' else c
    sanitize = map replace . dropWhile disallowed

postCtx :: Tags -> String -> Context String
postCtx tags root =
  dateField "date" "%B %e, %Y"
    <> markdownMetadataField root "description"
    <> tagsField "tags" tags
    <> baseContext
