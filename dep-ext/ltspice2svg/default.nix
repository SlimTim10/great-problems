{ pkgs, buildGoModule, fetchFromGitHub }:
buildGoModule rec {
  pname = "ltspice2svg";
  version = "0.8.3";

  src = fetchFromGitHub {
    owner = "icewire314";
    repo = "ltspice2svg";
    rev = "v${version}";
    sha256 = "sha256-70728voXCS0Qssr4qL7afgx2BPWfsL17ggiY0jQa0F0=";
  };

  # modSha256 = pkgs.stdenv.lib.fakeSha256;
  modSha256 = "sha256-C9DW1szZjwlth0R28oEKrnlw92wj2vgvz2Tgrt9wKgo=";
}
