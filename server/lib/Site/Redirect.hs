{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Site.Redirect where

import Data.Monoid (Endo (Endo, appEndo))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Wai (Application, pathInfo)
import Network.Wai.Middleware.Vhost (redirectTo)

type Redirect = (Text, Text)

applyRedirect :: Redirect -> Application -> Application
applyRedirect (from, to) app =
  let matchesFrom = (drop 1 (Text.splitOn "/" from) ==) . pathInfo
   in \req respond -> do
        if matchesFrom req
          then respond (redirectTo (Text.encodeUtf8 to))
          else app req respond

applyRedirects :: [Redirect] -> Application -> Application
applyRedirects = appEndo . foldMap (Endo . applyRedirect)

redirects :: Application -> Application
redirects =
  applyRedirects
    [ ("/content/about.html", "/about"),
      ("/content/posts.html", "/posts"),
      ("/content/posts/2011-06-01-design-futures.html", "/posts/design-futures"),
      ("/content/posts/2012-10-03-brisbane-bicycle-film-festival.html", "/posts/brisbane-bicycle-film-festival"),
      ("/content/posts/2013-02-08-petrichor.html", "/posts/petrichor"),
      ("/content/posts/2017-01-06-you-already-know-what-monads-are.html", "/posts/you-already-know-what-monads-are"),
      ("/content/posts/2018-08-21-lets-learn-about-lenses.html", "/posts/lets-learn-about-lenses"),
      ("/content/posts/2019-10-05-servant-types.html", "/posts/servant-types"),
      ("/content/posts/2019-11-21-deploying-a-fully-automated-nix-based-static-website.html", "/posts/deploying-a-fully-automated-nix-based-static-website"),
      ("/content/posts/2020-01-27-getting-close-to-the-conceptual-metal.html", "/posts/getting-close-to-the-conceptual-metal"),
      ("/content/posts/2020-02-12-home-sweet-home.html", "/posts/home-sweet-home"),
      ("/content/posts/2020-02-16-using-nix-to-deploy-a-haskell-web-app-to-heroku.html", "/posts/using-nix-to-deploy-a-haskell-web-app-to-heroku"),
      ("/content/resume.html", "/resume"),
      ("/tags/design.html", "/posts/tags/design"),
      ("/tags/development.html", "/posts/tags/development")
    ]
