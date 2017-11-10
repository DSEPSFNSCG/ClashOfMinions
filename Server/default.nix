with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "server";

  src = lib.cleanSource ./.;

  buildInputs = [
    (haskellPackages.ghcWithPackages (hp: with hp; [
      network-simple
      aeson
    ]))
  ];

  buildPhase = ''
    ghc Main.hs
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv Main $out/bin/server
  '';
}
