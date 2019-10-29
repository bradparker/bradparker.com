# Personal site

If you have [Nix](https://nixos.org/nix) and a recent-ish version of [cabal-install](https://www.haskell.org/cabal/) installed:

```
$ nix-shell ./builder -A env --run 'cabal new-run builder:builder watch'
```

To run a slide show.

```
$ nix-shell ./slides --run 'patat --watch slides/2019-10-05-servant-types.md'
```
