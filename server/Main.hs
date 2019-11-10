{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -threaded -rtsopts -with-rtsopts=-N #-}

module Main
  ( main
    )
where

import Control.Applicative (optional)
import Network.Wai (Application)
import Network.Wai.Application.Static
  ( StaticSettings (ssIndices),
    defaultWebAppSettings,
    staticApp
    )
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (Port)
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
    strOption
    )
import WaiAppStatic.Types (unsafeToPiece)

data HttpsOptions
  = HttpsOptions
      { keyFile :: String,
        certFile :: String
        }

data Options
  = Options
      { port :: Port,
        directory :: FilePath,
        https :: Maybe HttpsOptions
        }

optionsP :: Parser Options
optionsP =
  Options <$> portP <*> directoryP <*> optional httpsP
  where
    portP = option auto (long "port" <> metavar "PORT")
    directoryP = strOption (long "directory" <> metavar "DIRECTORY")
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

app :: FilePath -> Application
app path = staticApp staticSettings
  where
    defaults = defaultWebAppSettings path
    index = unsafeToPiece "index.html"
    staticSettings = defaults {ssIndices = [index]}

tlsSettings :: HttpsOptions -> WarpTLS.TLSSettings
tlsSettings httpsOpts =
  WarpTLS.defaultTlsSettings
    { WarpTLS.certFile = certFile httpsOpts,
      WarpTLS.keyFile = keyFile httpsOpts
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
  run options $ logStdout $ app $ directory options
