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
#    dotenv = dontCheck (self.callCabal2nix "dotenv"
#      (pkgs.fetchFromGitHub {
#        owner = "stackbuilders";
#        repo = "dotenv-hs";
#        rev = "b4cc2ac18d71a1e500f1ea8c180f723022b50651";
#        sha256 = "0vgj5wn4p9wrs56aah49b57p6v95xz5j57mabadnrarg5ybq6wcc";
#      }){});
    wreq = dontCheck (self.callCabal2nix "wreq"
      (pkgs.fetchFromGitHub {
        owner = "haskell";
        repo = "wreq";
        rev = "2da0070625a680d5af59e36c3ca253ef6a80aea9";
        sha256 = "1zdxpcs9hrjl5s9czd7y4hkqm2mn8gwcaha61r0crcgs8qj3sqfs";
      }){});
    dotenv = dontCheck (self.callCabal2nix "dotenv" (hackGet ./dep/dotenv-hs) {});
  });
  packages = {
    reflex-dom-ace = hackGet ./dep/reflex-dom-ace;
  };
})