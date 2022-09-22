{ pkgs, buildGoModule, fetchFromGitHub }:
buildGoModule rec {
  pname = "problem2tex";
  version = "0.9.14";

  src = fetchFromGitHub {
    owner = "icewire314";
    repo = "problem2tex";
    # rev = "v${version}";
    rev = "243ab93cce43373b6f4f1ee299aa86da7356c30f";
    # sha256 = pkgs.stdenv.lib.fakeSha256;
    sha256 = "sha256-Md3peaJHI0+pNNd+NCbQVtlGVEn5o0yj5ujl1+h5cAQ=";
  };

  # modSha256 = pkgs.stdenv.lib.fakeSha256;
  modSha256 = "sha256-FoAdAxJHyCL/EOEwwZjZWAuqpwmWHPJu6kyLYlFQ8kg=";
}
