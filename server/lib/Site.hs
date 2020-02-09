{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Site
  ( new,
    Options (..),
  )
where

import Network.HTTP.Types (status404)
import Network.Wai (Application, responseLBS)
import Network.Wai.Middleware.Static
  ( (<|>),
    (>->),
    CacheContainer,
    CachingStrategy (PublicStaticCaching),
    Policy,
    addBase,
    hasSuffix,
    initCaching,
    noDots,
    policy,
    predicate,
    unsafeStaticPolicy',
  )
import System.FilePath (dropDrive, hasExtension)

newtype Options
  = Options {directory :: FilePath}

appPolicy :: FilePath -> Policy
appPolicy path =
  noDots >-> (indexes <|> mempty) >-> dropLeadingSlash >-> addBase path
  where
    dropLeadingSlash = policy (Just . dropDrive)
    indexes = isIndex >-> addSuffix "index.html"
    isIndex = hasSuffix "/" <|> (noExtension >-> addSuffix "/")
    noExtension = predicate (not . hasExtension)
    addSuffix s = policy (Just . (++ s))

app :: Options -> CacheContainer -> Application
app (Options dir) cache =
  unsafeStaticPolicy' cache (appPolicy dir) notFound
  where
    notFound :: Application
    notFound _ respond =
      respond $
        responseLBS
          status404
          [("Content-Type", "text/plain")]
          "File not found"

new :: Options -> IO Application
new options =
  app options <$> initCaching PublicStaticCaching
