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
project ./. ({ pkgs, hackGet, ... }: {
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
    problem2tex = pkgs.callPackage ./dep-ext/problem2tex {};
    ltspice2svg = pkgs.callPackage ./dep-ext/ltspice2svg {};
  };
  
})
