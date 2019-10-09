module Main
  ( main
  )
where

import Network.Wai (Application)
import Network.Wai.Application.Static
  ( staticApp
  , defaultWebAppSettings
  )
import Network.Wai.Handler.Warp (run)

app :: FilePath -> Application
app = staticApp . defaultWebAppSettings

main :: IO ()
main = run 8080 $ app "_site"
