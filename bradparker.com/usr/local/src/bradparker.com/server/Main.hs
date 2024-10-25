{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Function ((&))
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.Wai.Cli (defWaiMain)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.Gzip
  ( GzipFiles (GzipIgnore, GzipPreCompressed),
    def,
    gzip,
    gzipFiles,
  )
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Site
import System.Environment (getEnv)
import System.FilePath ((</>))

getStartTime :: IO ByteString
getStartTime = ByteString.pack . formatRFCRFC2616 <$> getCurrentTime
  where
    formatRFCRFC2616 = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

getBuiltAt :: FilePath -> IO ByteString
getBuiltAt directory = ByteString.pack <$> readFile (directory </> "built-at")

main :: IO ()
main = do
  ssl <- getEnv "FORCE_SSL" <|> pure "false"
  webRoot <- getEnv "WEB_ROOT" <|> pure ""
  builtAt <- getBuiltAt webRoot <|> getStartTime
  app <- Site.new builtAt (Site.Options webRoot)
  app
    & logStdout
    & bool id forceSSL (ssl == "true")
    & gzip (def {gzipFiles = GzipPreCompressed GzipIgnore})
    & defWaiMain
