---
title: Using Nix to deploy a Haskell web app to Heroku
tags: development
description: |
  Deploying Ruby on Rails apps to Heroku has always been a delight, and now that they have a container registry other run-times are similarly delightfully deployable. No build-packs required.

  ```nix
  with (import <nixpkgs> {});
  dockerTools.buildImage {
    name = "hello";
    contents = [ hello ];
  }
  ```
---

Let's say we've written a web app in Haskell.

```
$ curl http://localhost:8000/hello/World
Hello, World!

$ curl http://localhost:8000/hello/Haskell
Hello, Haskell!
```

It sure would be nice if we could share it with other people. Let's deploy this wonderful thing to the _internet_ using Heroku.

## Haskell and Nix

Thanks to the [Nix](https://nixos.org/nix/) using Haskell community packaging up libraries and applications is very convenient.

First, we'll make a directory for our project.

```
~ $ mkdir haskell-on-heroku
~ $ cd $_
~/haskell-on-heroku $
```

Then we can ask Cabal to get us started with a scaffold.

```
~/haskell-on-heroku $ nix run \
> nixpkgs.cabal-install \
> --command cabal init \
> --minimal \
> --cabal-version=2.4
```

Finally, we add a `default.nix` which makes use of [`cabal2nix`](https://github.com/NixOS/cabal2nix).

```nix
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages
}:
  haskellPackages
    .callCabal2nix "haskell-on-heroku" ./. {}
```

And it's now ready to try out.

```
~/haskell-on-heroku $ nix run \
> --file default.nix \
> env --command cabal new-run
# ...
Hello, Haskell!
```

## An amazing web application

Our web server uses the nifty Haskell library [Servant server](https://hackage.haskell.org/package/servant-server) so we'll need to add it and [Warp](https://hackage.haskell.org/package/warp) as dependancies to `haskell-on-heroku.cabal`.

```diff
 executable haskell-on-heroku
   main-is:             Main.hs
   build-depends:       base >=4.12 && <4.13
+                     , servant-server
+                     , warp
   default-language:    Haskell2010
```

We'll probably also need to put the implementation of our app in `Main.hs`.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative ((<|>))
import Data.Proxy (Proxy (Proxy))
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:>),
    Capture,
    Get,
    Handler,
    PlainText,
    serve,
  )
import System.Environment (getEnv)

type Greeter =
  "hello"
    :> Capture "name" String
    :> Get '[PlainText] String

greet :: String -> Handler String
greet name = 
  pure $ "Hello, " <> name <> "!"

getPort :: IO Int
getPort =
  read <$> getEnv "PORT" <|> pure 8000

main :: IO ()
main = do
  port <- getPort
  run port $ serve (Proxy @Greeter) greet
```

## A deployable image

Both the Haskell and Docker using members of the Nix community have made building images out of Haskell packages very convenient.

```nix
{ name, nixpkgs ? import <nixpkgs> {} }:
let
  inherit nixpkgs
    busybox
    callPackage
    dockerTools
    haskell
    lib;
    
  package = lib.pipe 
    (callPackage ./. {}) 
    (with haskell.lib; [
      dontCheck
      justStaticExecutables
    ]);
in
  dockerTools.buildImage {
    name = name;
    tag = "latest";
    contents = [
      busybox
      package
    ];
    config = {
      Cmd = ["/bin/${package.pname}"];
    };
  }
```

Using the [`callPackage`](https://github.com/NixOS/nixpkgs/blob/f9be656873dacbc5f51f0cea41e5c4ea0f358b2b/lib/customisation.nix#L117) helper means were less likely to accidentally end up with multiple versions of the Nix packages set. If that should happen, our app would still run fine, we just might end up with a bigger than necessary image.

[`lib.pipe`](https://github.com/NixOS/nixpkgs/blob/master/lib/trivial.nix#L61) allows us to perform a series of modifications on a value. We use it with [`dontCheck`]() and [`justStaticExecutables`]() from [`haskell.lib`]() to modify our Haskell package's configuration to make for a smaller image.

With [`dockerTools.buildImage`]() we can build a minimalistic docker image out of a set of Nix derivations.

We need [`busybox`]() because of how Heroku actually starts containers.
