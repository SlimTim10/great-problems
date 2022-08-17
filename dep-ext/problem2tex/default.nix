{ pkgs }:
pkgs.buildGoModule rec {
  pname = "problem2tex";
  version = "0.9.13";

  src = pkgs.fetchFromGitHub {
    owner = "icewire314";
    repo = "problem2tex";
    rev = "v${version}";
    sha256 = "sha256-wjkwU91fqdS5qQeGXnfZuXwAP8ndqcg9fywCVSwQ4FQ=";
  };

  # modSha256 = pkgs.stdenv.lib.fakeSha256;
  modSha256 = "sha256-C9DW1szZjwlth0R28oEKrnlw92wj2vgvz2Tgrt9wKgo=";
}
