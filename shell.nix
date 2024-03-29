{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let 
    newHaskellPackages = haskellPackages;
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              attoparsec cabal-install lens split text monad-loops
              tasty tasty-hunit transformers-either
            ]);
in stdenv.mkDerivation {
  name = "textview-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
