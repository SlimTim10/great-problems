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
      rev = "5720581e5880685a6c48964aa2680d610e380f59";
      sha256 = "10lrbxrnzr3yck4psxhcm5cccm61mj22qwk7vlcbyd7akvfzk631";
    };
  in
  {
    reflex-dom-ace = self.callCabal2nix "reflex-dom-ace" aceSrc {};
  };
})
