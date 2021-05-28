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
  overrides = self: super: rec
  {
    reflex-dom-ace = self.callHackageDirect {
      pkg = "reflex-dom-ace";
      ver = "0.3.0.3";
      sha256 = "01kff9cn08hyw1k5bnwvc8x406bclpxmkcb61yss3xxrd0ffnpzm";
    } {};

    dotenv = pkgs.haskell.lib.dontCheck (self.callHackageDirect {
      pkg = "dotenv";
      ver = "0.8.0.7";
      sha256 = "0rhawjglimx3xg4s78h99n2gg3ydjbinbx30gyan1vfzvhi1akyl";
    } {});

  };
})
