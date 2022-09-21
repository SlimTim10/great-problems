with import <nixpkgs> {}; with buildGo117Module;
{ pkgs, fetchFromGitHub }:
buildGo117Module rec {
  pname = "problem2tex";
  version = "0.9.14";

  src = fetchFromGitHub {
    owner = "SlimTim10";
    repo = "problem2tex";
    # rev = "v${version}";
    rev = "410fd6f1aea93f587415ad3e0249b6f42fc26963";
    # sha256 = pkgs.stdenv.lib.fakeSha256;
    sha256 = "sha256-iR02O6d32F/xMGQfel1GVSt3hMc7kGBU1BBJhoGDYAw=";
  };

  # vendorSha256 = pkgs.stdenv.lib.fakeSha256;
  vendorSha256 = "sha256-jX68SRCQbinIV45t3/DBAA5oLi87K7BSdbno116LRlY=";
}
