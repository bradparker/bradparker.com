# Personal site

If you have [Nix](https://nixos.org/nix) installed:

```
$ nix-shell -A bradparker-com.builder.env --run 'SOURCE_DIR=site runhaskell builder/Main.hs watch'
```

To test the production server

```
$ nix-shell -A bradparker-com.builder.env --run 'SOURCE_DIR=site runhaskell builder/Main.hs build'
$ nix-shell -A bradparker-com.server.env --run 'runhaskell server/Main.hs --port 8080 --directory _site'
```
