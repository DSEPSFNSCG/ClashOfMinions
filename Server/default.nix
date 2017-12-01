let
  nixpkgsRev = "2f1a818d00f957f3102c0b412864c63b6e3e7447";

  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = nixpkgsRev;
    sha256 = "1g9yvbkayjv4w9sa99g2zfys4kq9mrp3fznfm6qy0n5h4kqc0ifd";
  };
in with import nixpkgs {};

haskell.lib.buildStackProject {
  name = "Test";
  src = ./.;
  buildInputs = [ ];
}
