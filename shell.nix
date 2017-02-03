{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bytestring, containers
      , cryptonite, directory, free, mtl, shake, split, stdenv
      , template-haskell, temporary, transformers, unix
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
