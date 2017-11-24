let
  nixpkgsRev = "17.09";

  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = nixpkgsRev;
    sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
  };

  pkgs = import nixpkgs {};
in
with pkgs;
haskell.lib.buildStackProject {
  name = "Test";
  buildInputs = [ ];
}
