{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Control.Applicative (optional)
import Network.Wai (Application)
import Network.Wai.Application.Static
  ( StaticSettings(ssIndices)
  , defaultWebAppSettings
  , staticApp
  )
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Options.Applicative
  ( Parser
  , auto
  , execParser
  , fullDesc
  , help
  , info
  , long
  , metavar
  , option
  , strOption
  )
import WaiAppStatic.Types (unsafeToPiece)

data HttpsOptions = HttpsOptions
  { keyFile :: String
  , certFile :: String
  }

data Options = Options
  { port :: Port
  , directory :: FilePath
  , https :: Maybe HttpsOptions
  }

optionsP :: Parser Options
optionsP =
  Options <$> portP <*> directoryP <*> optional httpsP
 where
  portP =
    option auto (long "port" <> metavar "PORT")
  directoryP =
    strOption (long "directory" <> metavar "DIRECTORY")
  httpsP =
    HttpsOptions
      <$> strOption
            (  long "https-key-file"
            <> metavar "HTTPS_KEY_FILE"
            )
      <*> strOption
            (  long "https-cert-file"
            <> metavar "HTTPS_CERT_FILE"
            )

app :: FilePath -> Application
app path = staticApp settings
 where
  defaults = defaultWebAppSettings path
  index    = unsafeToPiece "index.html"
  settings = defaults { ssIndices = [index] }

main :: IO ()
main = do
  options <- execParser $ info optionsP fullDesc
  let settings =
        Warp.setPort (port options) Warp.defaultSettings
  case https options of
    Nothing ->
      Warp.runSettings settings $ app (directory options)
    Just httpsOpts -> do
      let tlsSettings = WarpTLS.defaultTlsSettings
            { WarpTLS.certFile = certFile httpsOpts
            , WarpTLS.keyFile  = keyFile httpsOpts
            }
      WarpTLS.runTLS tlsSettings settings
        $ app (directory options)
