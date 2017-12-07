{ mkDerivation, base, bytestring, either, lens, split, stdenv, text
, transformers, attoparsec, containers
, tasty, tasty-hunit
}:
mkDerivation {
  pname = "textview";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring either lens split text transformers attoparsec
  ];
  testHaskellDepends = [
    base containers text tasty tasty-hunit
  ];
  license = "unknown";
}
