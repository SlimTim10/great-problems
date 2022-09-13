{ pkgs, buildGoModule, fetchFromGitHub }:
buildGoModule rec {
  pname = "problem2tex";
  version = "0.9.14";

  src = fetchFromGitHub {
    owner = "icewire314";
    repo = "problem2tex";
    # rev = "v${version}";
    rev = "a03359d5f297e224a29704b88df782a6b409ed5f";
    # sha256 = pkgs.stdenv.lib.fakeSha256;
    sha256 = "sha256-9Ne545st4NLofe+PFs8TRm2c9scAM9PhdGBqHg2p7EQ=";
  };

  # modSha256 = pkgs.stdenv.lib.fakeSha256;
  modSha256 = "sha256-C9DW1szZjwlth0R28oEKrnlw92wj2vgvz2Tgrt9wKgo=";
}
