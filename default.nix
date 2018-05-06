{ mkDerivation, attoparsec, base, bytestring, containers, lens, mtl
, split, stdenv, tasty, tasty-hunit, text, transformers
, transformers-either
}:
mkDerivation {
  pname = "textview";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers lens mtl split text
    transformers transformers-either
  ];
  testHaskellDepends = [ base containers tasty tasty-hunit text ];
  license = "unknown";
}
