with import <nixpkgs> {}; with buildGo117Module;
{ pkgs, fetchFromGitHub }:
buildGo117Module rec {
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

  # vendorSha256 = pkgs.stdenv.lib.fakeSha256;
  vendorSha256 = "sha256-jX68SRCQbinIV45t3/DBAA5oLi87K7BSdbno116LRlY=";
}
