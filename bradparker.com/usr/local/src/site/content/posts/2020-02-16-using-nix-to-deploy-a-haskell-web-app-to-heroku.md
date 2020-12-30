---
title: Using Nix to deploy a Haskell web app to Heroku
rss_guid: https://bradparker.com/content/posts/2020-02-16-using-nix-to-deploy-a-haskell-web-app-to-heroku.html
tags:
  - development
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

Thanks to the [Nix](https://nixos.org/nix/) using Haskell community packaging up Haskell libraries and executables is very convenient.

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
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
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

The Docker-using members of the Nix community have made building docker-compatible images from Nix derivations very convenient.

To try it out put the following in a file somewhere, say `~/hello.nix`.

```nix
let
  nixpkgs = import <nixpkgs> {};
in
  nixpkgs.dockerTools.buildImage {
    name = "hello-docker-nix";
    tag = "latest";
    contents = [ nixpkgs.hello ];
  }
```

Then build it.

```
~ $ nix-build hello.nix
```

Load the result.

```
~ $ docker load < result
```

And finally run it.

```
~ $ docker run hello-docker-nix:latest hello
Hello, world!
```

We can put our package in a docker-compatible image in a similar fashion. Put the following in a file called `release.nix` in your `haskell-on-heroku` project directory.

```nix
{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs)
    callPackage
    dockerTools;

  package = callPackage ./. {};
in
  dockerTools.buildImage {
    name = "haskell-on-heroku";
    tag = "latest";
    contents = [
      package
    ];
  }
```

Using the [`callPackage`](https://github.com/NixOS/nixpkgs/blob/f9be656873dacbc5f51f0cea41e5c4ea0f358b2b/lib/customisation.nix#L117) helper means we're less likely to accidentally end up with multiple versions of the Nix packages set. If that should happen, our app would still run fine, we just might end up with a bigger than necessary image.

Let's try to build it and see how big it is.

```
~/haskell-on-heroku $ nix-build release.nix
# ... build output
~/haskell-on-heroku $ docker load < result
3f5a871dd9ee: Loading layer   38.1MB/38.1MB
Loaded image: haskell-on-heroku:latest
```

It's about 38.1 megabytes. That's not bad.

Now, I'm going to save us some trouble and add [`busybox`](https://www.busybox.net/) to our image and also make sure it has a `Cmd` configured.

```diff
diff --git a/release.nix b/release.nix
index 8fa2527..e14a534 100644
--- a/release.nix
+++ b/release.nix
@@ -3,7 +3,8 @@
 let
   inherit (nixpkgs)
     callPackage
-    dockerTools;
+    dockerTools
+    busybox;

   package = callPackage ./. {};
 in
@@ -12,5 +13,9 @@ in
     tag = "latest";
     contents = [
       package
+      busybox
     ];
+    config = {
+      Cmd = ["/bin/${package.pname}"];
+    };
   }
```

We need to specify a command so Heroku knows how to run the container it'll create from our image. We need `busybox` because Heroku will attempt to run our image's command with `bash -c`. Images built with Nix's `dockerTools` are so minimal they don't have `bash` or even `/bin/sh`.

## Shipping it

We now have everything we need. The last steps are to create a Heroku application, push our image to the Heroku registry and release it.

First, assuming you already have a Heroku account, log into the Heroku command line.

```
~/haskell-on-heroku $ nix run nixpkgs.heroku \
> --command heroku login
```

Then also login into the container registry.

```
~/haskell-on-heroku $ nix run nixpkgs.heroku \
> --command heroku container:login
```

Next create an app.

```
~/haskell-on-heroku $ nix run nixpkgs.heroku \
> --command heroku create
```

Build our image.

```
~/haskell-on-heroku $ nix-build release.nix
```

Load it.

```
~/haskell-on-heroku $ docker load < result
```

Tag it with our Heroku application's name and the process type we'd like to run it as.

```
~/haskell-on-heroku $ docker tag \
> haskell-on-heroku \
> registry.heroku.com/infinite-anchorage-09330/web
```

Push it to the registry.

```
~/haskell-on-heroku $ docker push \
> registry.heroku.com/infinite-anchorage-09330/web
```

And, lastly, release it.

```
~/haskell-on-heroku $ nix run nixpkgs.heroku \
> heroku container:release -a infinite-anchorage-09330 web
```

Hey presto, our amazing application will now be accessible by the whole world. How cool.

That does seem like a lot to remember, however, so we'd best pop as much of it as we can in a script. A script we might call `./deploy`.

```bash
#!/usr/bin/env bash

set -exu

app_name=$1
result=$(nix-build --no-out-link release.nix)

docker load < $result
docker tag $app_name registry.heroku.com/$app_name/web
docker push registry.heroku.com/$app_name/web

nix run nixpkgs.heroku --command \
  heroku container:release -a $app_name web
```

With that building and deployment is a little more manageable.

```
~/haskell-on-heroku $ ./deploy infinite-anchorage-09330
```

***

I've prepared a [repository](https://github.com/bradparker/haskell-on-heroku) of everything we've gone through here as a reference. With any luck it may help someone get their awesome Haskell application onto the mad expanse that is the internet.
