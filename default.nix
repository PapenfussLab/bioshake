{ mkDerivation, base, binary, bytestring, containers, cryptonite
, directory, free, mtl, shake, split, stdenv, template-haskell
, temporary, transformers, unix
}:
mkDerivation {
  pname = "bioshake";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers cryptonite directory free mtl
    shake split template-haskell temporary transformers unix
  ];
  description = "Bioinformatics pipelines with shake";
  license = stdenv.lib.licenses.isc;
}
