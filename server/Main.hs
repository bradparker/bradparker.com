{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -threaded -rtsopts -with-rtsopts=-N #-}

module Main
  ( main
    )
where

import Control.Applicative (optional)
import Network.HTTP.Types (status404)
import Network.Wai (Application, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
  ( (<|>),
    (>->),
    CacheContainer,
    CachingStrategy (PublicStaticCaching),
    Policy,
    addBase,
    hasSuffix,
    initCaching,
    policy,
    predicate,
    staticPolicy'
    )
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
import System.FilePath (dropDrive, hasExtension)

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

appPolicy :: FilePath -> Policy
appPolicy path =
  dropLeadingSlash >-> addBase path >-> (indexes <|> mempty)
  where
    dropLeadingSlash = policy (Just . dropDrive)
    indexes = isIndex >-> addSuffix "index.html"
    isIndex = hasSuffix "/" <|> (noExtension >-> addSuffix "/")
    noExtension = predicate (not . hasExtension)
    addSuffix s = policy (Just . (++ s))

app :: FilePath -> CacheContainer -> Application
app path cache =
  staticPolicy' cache (appPolicy path) notFound
  where
    notFound :: Application
    notFound _ respond =
      respond
        $ responseLBS status404
            [("Content-Type", "text/plain")]
            "File not found"

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
  cache <- initCaching PublicStaticCaching
  run options $ logStdout $ app (directory options) cache
