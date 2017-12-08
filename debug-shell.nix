{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay    ? <uphere-nix-overlay>
, HCoreNLP              ? <HCoreNLP>
, wiki-ner              ? <wiki-ner>
, nlp-types             ? <nlp-types>
, graph-algorithms      ? <graph-algorithms>
}:


let newpkgs = import pkgs.path {
                overlays = [ (self: super: {
                               libsvm = import (uphere-nix-overlay + "/nix/cpp-modules/libsvm/default.nix") { inherit (self) stdenv fetchurl; };
                             })
                           ];
              };
in

with newpkgs;


let
  fasttext = import (uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit stdenv fetchgit; };
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;
  config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { pkgs = newpkgs; };
  haskellPackages1 = haskellPackages.override { overrides = config1; };
  fastTextNix = import (semantic-role-labeler + "/fasttext/default.nix") {
    inherit stdenv;
    haskellPackages = haskellPackages1;
  };
  config2 =
    self: super: {
      "HCoreNLP"              = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "wiki-ner"              = self.callPackage (import wiki-ner) {};
      "nlp-types"             = self.callPackage (import nlp-types) {};
      "HCoreNLP-Proto"        = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
      "graph-algorithms"      = self.callPackage (import graph-algorithms) {};
      };
  newHaskellPackages =  haskellPackages.override {
    overrides = self: super: config1 self super // config2 self super;
  };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              attoparsec cabal-install either lens split text monad-loops p.HCoreNLP p.wiki-ner p.nlp-types
            ]);
in stdenv.mkDerivation {
  name = "textview-dev";
  buildInputs = [ hsenv jdk ];
  shellHook = ''
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}
