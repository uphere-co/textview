{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let 
    newHaskellPackages = haskellPackages;
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              attoparsec lens split text monad-loops
            ]);
in stdenv.mkDerivation {
  name = "textview-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
