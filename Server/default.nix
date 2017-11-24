let
  nixpkgsRev = "cfafd6f5a819472911eaf2650b50a62f0c143e3e";

  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = nixpkgsRev;
    sha256 = "10xgiyh4hbwwiy8qg70ma1f27nd717aflksk9fx3ci8bmxmqbkkn";
  };

  pkgs = import nixpkgs {};
in
with pkgs;
haskell.lib.buildStackProject {
  name = "Test";
  src = ./.;
  buildInputs = [ ];
}
