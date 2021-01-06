{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Applicative (optional)
import Data.Foldable (for_)
import Data.Function ((&))
import qualified Network.TLS.Extra as TLSExtra
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Systemd as WarpSystemd
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Network.Wai.Middleware.RequestLogger (logStdout)
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    info,
    long,
    metavar,
    option,
    strOption,
  )
import qualified Site
import System.Posix.Signals (Handler (Default), installHandler, sigINT, sigTERM)

data HttpsOptions = HttpsOptions
  { keyFile :: String,
    certFile :: String
  }

data Options = Options
  { port :: Port,
    site :: Site.Options,
    https :: Maybe HttpsOptions
  }

optionsP :: Parser Options
optionsP =
  Options <$> portP <*> siteP <*> optional httpsP
  where
    portP = option auto (long "port" <> metavar "PORT")
    siteP =
      Site.Options
        <$> strOption
          ( long "site-directory"
              <> metavar "STATIC_DIRECTORY"
          )
    httpsP =
      HttpsOptions
        <$> strOption
          ( long "https-key-file"
              <> metavar "HTTPS_KEY_FILE"
          )
        <*> strOption
          ( long "https-cert-file"
              <> metavar "HTTPS_CERT_FILE"
          )

tlsSettings :: HttpsOptions -> WarpTLS.TLSSettings
tlsSettings httpsOpts =
  ( WarpTLS.tlsSettings
      (certFile httpsOpts)
      (keyFile httpsOpts)
  )
    { WarpTLS.tlsCiphers = TLSExtra.ciphersuite_default
    }

settings :: Options -> Warp.Settings
settings options =
  Warp.defaultSettings
    & Warp.setPort (port options)
    & Warp.setInstallShutdownHandler shutdown
    & Warp.setGracefulShutdownTimeout (Just 30)
  where
    shutdown :: IO () -> IO ()
    shutdown _ = for_ [sigTERM, sigINT] \sig ->
      installHandler sig Default Nothing

run :: Options -> Application -> IO ()
run options app =
  case https options of
    Nothing ->
      WarpSystemd.runSystemdWarp
        WarpSystemd.defaultSystemdSettings
        (settings options)
        app
    Just httpsOptions ->
      WarpSystemd.runSystemdWarpTLS
        WarpSystemd.defaultSystemdSettings
        (tlsSettings httpsOptions)
        (settings options)
        app

main :: IO ()
main = do
  options <- execParser $ info optionsP fullDesc
  siteApp <- Site.new (site options)
  run options $ logStdout siteApp
