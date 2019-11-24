# Personal site

If you have [Nix](https://nixos.org/nix) installed:

```
$ site/run watch
```

To test the production server

```
$ site/run build
$ nix-shell -A bradparker-com.server.env --run 'runhaskell server/Main.hs --port 8080 --directory _site'
```
