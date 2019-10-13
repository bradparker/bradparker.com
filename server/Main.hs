{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Network.Wai (Application)
import Network.Wai.Application.Static
  ( StaticSettings(ssIndices)
  , defaultWebAppSettings
  , staticApp
  )
import Network.Wai.Handler.Warp (run)
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
import WaiAppStatic.Types
  ( unsafeToPiece
  )

type Port = Int

data Options = Options
  { port :: Port
  , directory :: FilePath
  }

optionsP :: Parser Options
optionsP =
  Options <$> portP <*> directoryP
 where
  portP = option
    auto
    (  long "port"
    <> metavar "PORT"
    <> help "Port"
    )
  directoryP = strOption
    (  long "directory"
    <> metavar "DIRECTORY"
    <> help "Directory"
    )

app :: FilePath -> Application
app path = staticApp settings
 where
  defaults = defaultWebAppSettings path
  index    = unsafeToPiece "index.html"
  settings =
    defaults { ssIndices = [index] }

main :: IO ()
main = do
  options <- execParser
    $ info optionsP fullDesc
  run (port options)
    $ app (directory options)
