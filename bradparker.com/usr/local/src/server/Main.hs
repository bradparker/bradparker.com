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

module Main
  ( main,
  )
where

import Control.Applicative (empty, optional, (<|>))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
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
    str,
  )
import qualified Site
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Environment = Environment
  { port :: Maybe Port,
    site :: Maybe Site.Options,
    https :: Maybe HttpsOptions
  }

env :: Read a => String -> IO (Maybe a)
env key = (readMaybe =<<) <$> lookupEnv key

getEnvironment :: IO Environment
getEnvironment =
  Environment
    <$> env "PORT"
    <*> runMaybeT
      ( Site.Options
          <$> MaybeT (lookupEnv "SITE_DIRECTORY")
      )
    <*> runMaybeT
      ( HttpsOptions
          <$> MaybeT (lookupEnv "HTTPS_KEY_FILE")
          <*> MaybeT (lookupEnv "HTTPS_CERT_FILE")
      )

data HttpsOptions = HttpsOptions
  { keyFile :: String,
    certFile :: String
  }

data Options = Options
  { port :: Port,
    site :: Site.Options,
    https :: Maybe HttpsOptions
  }

maybeToParser :: Maybe a -> Parser a
maybeToParser = maybe empty pure

optionsP :: Environment -> Parser Options
optionsP environment =
  Options
    <$> (portP <|> maybeToParser environment.port)
    <*> (siteP <|> maybeToParser environment.site)
    <*> optional (httpsP <|> maybeToParser environment.https)
  where
    portP :: Parser Port
    portP = option auto (long "port" <> metavar "PORT")

    siteP :: Parser Site.Options
    siteP =
      Site.Options
        <$> option
          str
          ( long "site-directory"
              <> metavar "SITE_DIRECTORY"
          )

    httpsP :: Parser HttpsOptions
    httpsP =
      HttpsOptions
        <$> option
          str
          ( long "https-key-file"
              <> metavar "HTTPS_KEY_FILE"
          )
        <*> option
          str
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
settings options = Warp.setPort options.port Warp.defaultSettings

run :: Options -> Application -> IO ()
run options app =
  case options.https of
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
  environment <- getEnvironment
  options <- execParser (info (optionsP environment) fullDesc)
  siteApp <- Site.new (options.site)
  run options (logStdout siteApp)
