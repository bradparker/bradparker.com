# Personal site

If you have [Nix](https://nixos.org/nix) and a recent-ish version of [cabal-install](https://www.haskell.org/cabal/) installed:

```
$ nix-shell ./builder -A env --run 'runhaskell builder/Main.hs watch'
```

To test the production server

```
$ nix-shell ./builder -A env --run 'runhaskell builder/Main.hs build'
$ nix-shell ./server -A env --run 'runhaskell server/Main.hs --port 8080 --directory _site'
```
