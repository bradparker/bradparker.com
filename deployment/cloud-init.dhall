{ write_files =
    [ { path =
          "/etc/nixos/host.nix"
      , permissions =
          "0644"
      , content =
          ./host.nix as Text
      }
    ]
, runcmd =
    [ "curl https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect | PROVIDER=digitalocean NIXOS_IMPORT=./host.nix NIX_CHANNEL=nixos-19.09 bash 2>&1 | tee /tmp/infect.log"
    ]
}
