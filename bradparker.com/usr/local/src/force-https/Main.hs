{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Network.Wai (Application, Request (rawPathInfo, requestHeaderHost))
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Vhost (redirectTo)
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    info,
    long,
    metavar,
    option,
  )

newtype Options = Options {port :: Port}

optionsP :: Parser Options
optionsP =
  Options <$> portP
  where
    portP = option auto (long "port" <> metavar "PORT")

hostname :: Request -> ByteString
hostname = fromMaybe "" . requestHeaderHost

redirectApp :: Application
redirectApp request respond =
  respond $
    redirectTo $
      "https://"
        <> hostname request
        <> rawPathInfo request

main :: IO ()
main = do
  options <- execParser $ info optionsP fullDesc
  run (port options) redirectApp
