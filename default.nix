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
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  overrides = self: super: let
    aceSrc = pkgs.fetchFromGitHub {
      owner = "SlimTim10";
      repo = "reflex-dom-ace";
      rev = "fa4d6bc6adcd9cf86f65be19e66a17517c35dfaa";
      sha256 = "0w0w53izjqbjjwfsr681kb1nani7lqydpwwjk8hcy9nnl6qxa3cm";
    };
  in
  {
    reflex-dom-ace = self.callCabal2nix "reflex-dom-ace" aceSrc {};
  };
})
