# Personal site

If you have [Nix](https://nixos.org/nix) and a recent-ish version of [cabal-install](https://www.haskell.org/cabal/) installed:

```
$ nix-shell ./builder -A env --run 'cabal new-run builder:builder watch'
```
