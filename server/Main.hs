{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Control.Applicative (optional)
import qualified Network.TLS.Extra as TLSExtra
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
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
  WarpTLS.defaultTlsSettings
    { WarpTLS.certFile = certFile httpsOpts,
      WarpTLS.keyFile = keyFile httpsOpts,
      WarpTLS.tlsCiphers = TLSExtra.ciphersuite_default
    }

runTLS :: HttpsOptions -> Warp.Settings -> Application -> IO ()
runTLS httpsOpts = WarpTLS.runTLS (tlsSettings httpsOpts)

settings :: Options -> Warp.Settings
settings options = Warp.setPort (port options) Warp.defaultSettings

run :: Options -> Application -> IO ()
run options =
  maybe
    Warp.runSettings
    runTLS
    (https options)
    (settings options)

main :: IO ()
main = do
  options <- execParser $ info optionsP fullDesc
  siteApp <- Site.new (site options)
  run options $ logStdout siteApp
