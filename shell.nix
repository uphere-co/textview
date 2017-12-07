{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let 
    newHaskellPackages = haskellPackages;
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              attoparsec cabal-install either lens split text monad-loops
              tasty tasty-hunit
            ]);
in stdenv.mkDerivation {
  name = "textview-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
