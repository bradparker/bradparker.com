{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
    )
where

import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Network.Wai (Application, Request (pathInfo, rawPathInfo, requestHeaderHost), responseLBS)
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.Vhost (redirectTo, vhost)
import Options.Applicative (Parser, auto, execParser, fullDesc, help, info, long, metavar, option, strOption)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.HTTP.Types (status404)

data Options = Options { port :: Port, directory :: FilePath }

optionsP :: Parser Options
optionsP =
  Options <$> portP <*> directoryP
  where
    portP = option auto (long "port" <> metavar "PORT")
    directoryP = strOption (long "directory" <> metavar "DIRECTORY")

challengeApp :: FilePath -> Application
challengeApp path = staticPolicy (addBase path) notFound
  where
    notFound _ respond =
      respond $ responseLBS status404 [] ""

hostname :: Request -> ByteString
hostname = fromMaybe "" . requestHeaderHost

redirectApp :: Application
redirectApp request respond =
  respond
    $ redirectTo
    $ "https://"
    <> hostname request
    <> rawPathInfo request

isChallenge :: Request -> Bool
isChallenge =
  ([".well-known", "acme-challenge"] ==) . take 2 . pathInfo

app :: FilePath -> Application
app path =
  vhost [(isChallenge, challengeApp path)] redirectApp

main :: IO ()
main = do
  options <- execParser $ info optionsP fullDesc
  run (port options) $ logStdoutDev $ app $ directory options
