{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Function ((&))
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

main :: IO ()
main = do
  ssl <- getEnv "FORCE_SSL" <|> pure "false"
  webRoot <- getEnv "WEB_ROOT" <|> pure ""
  app <- Site.new (Site.Options webRoot)
  app
    & logStdout
    & bool id forceSSL (ssl == "true")
    & gzip (def {gzipFiles = GzipPreCompressed GzipIgnore})
    & defWaiMain
