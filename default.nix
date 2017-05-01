{ mkDerivation, base, bytestring, lens, split, stdenv, text
, transformers, attoparsec
}:
mkDerivation {
  pname = "textview";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring lens split text transformers attoparsec
  ];
  license = "unknown";
}
