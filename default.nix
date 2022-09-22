{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }:
  let
    # unstable = import (builtins.fetchGit {
    #   name = "nixos-unstable-2022-09-21";
    #   url = "https://github.com/nixos/nixpkgs/";
    #   # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
    #   ref = "refs/heads/nixos-unstable";
    #   rev = "f677051b8dc0b5e2a9348941c99eea8c4b0ff28f";
    # }) {};
    unstable = import (builtins.fetchTarball {
      name = "nixos-unstable-2022-09-21";
      url = "https://github.com/nixos/nixpkgs/archive/f677051b8dc0b5e2a9348941c99eea8c4b0ff28f.tar.gz";
      # Hash obtained using `nix-prefetch-url --unpack <url>`
      sha256 = "18zycb8zxnz20g683fgbvckckr7rmq7c1gf96c06fp8pmaak0akx";
    }) {};
    buildGo117Module = pkgs.buildGoModule.override {
      go = unstable.go_1_17;
    };
  in
    {
      overrides = with pkgs.haskell.lib; (self: super: {
        dotenv = dontCheck (self.callCabal2nix "dotenv" (hackGet ./dep/dotenv-hs) {});
        wreq = dontCheck (self.callCabal2nix "wreq" (hackGet ./dep/wreq) {});
      });
      
      packages = {
        reflex-dom-ace = hackGet ./dep/reflex-dom-ace;
      };
      
      shellToolOverrides = self: super: {
        # Examples
        # cowsay = pkgs.cowsay;
        # echotest = pkgs.writeShellScriptBin "echotest" ''echo test'';
        
        # emacs-27.2
        emacs = (import (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/7d5956bf56e1f300c6668704b5e9c4cd8f5296cd.tar.gz";
        }) {}).emacs;
        problem2tex = pkgs.callPackage ./dep-ext/problem2tex { buildGoModule = buildGo117Module; };
        ltspice2svg = pkgs.callPackage ./dep-ext/ltspice2svg { buildGoModule = buildGo117Module; };
      };
      
    })
