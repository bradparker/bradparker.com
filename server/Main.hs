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
import WaiAppStatic.Types (unsafeToPiece)

app :: Application
app = staticApp $ (defaultWebAppSettings "_site")
  { ssIndices = [unsafeToPiece "index.html"]
  }

main :: IO ()
main = run 8080 app
