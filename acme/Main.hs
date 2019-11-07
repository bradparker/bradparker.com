{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
    )
where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Network.Wai (Application, Request (rawPathInfo, requestHeaderHost))
import Network.Wai.Handler.Warp (Port, Settings, defaultSettings, runSettings, setHost, setPort)
import Network.Wai.Middleware.Static ((>->), Policy, addBase, hasPrefix, staticPolicy)
import Network.Wai.Middleware.Vhost (redirectTo)
import Options.Applicative (Parser, auto, execParser, fullDesc, info, long, metavar, option, strOption)

data Options = Options {port :: Port, directory :: FilePath}

optionsP :: Parser Options
optionsP =
  Options <$> portP <*> directoryP
  where
    portP = option auto (long "port" <> metavar "PORT")
    directoryP = strOption (long "directory" <> metavar "DIRECTORY")

policy :: String -> Policy
policy path = hasPrefix ".well-known/acme-challenge" >-> addBase path

app :: FilePath -> Application
app path = staticPolicy (policy path) redirectApp

hostname :: Request -> ByteString
hostname = fromMaybe "" . requestHeaderHost

redirectApp :: Application
redirectApp request respond =
  respond
    $ redirectTo
    $ "https://"
    <> hostname request
    <> rawPathInfo request

settings :: Options -> Settings
settings options =
  setPort (port options)
    $ setHost "127.0.0.1"
        defaultSettings

main :: IO ()
main = do
  options <- execParser $ info optionsP fullDesc
  runSettings (settings options) $ app $ directory options
