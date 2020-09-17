{ mkDerivation
, base
, hashmap
, hpack
, HUnit
, llvm-hs
, llvm-hs-pretty
, megaparsec
, optparse-applicative
, stdenv
, tasty
, tasty-hunit
, text
}:

mkDerivation {
  pname = "compiler";
  version = "0.0.1";

  src = ./.;

  isLibrary = false;
  isExecutable = true;

  libraryToolDepends = [
    hpack
  ];

  executableHaskellDepends = [
    base
    hashmap
    llvm-hs
    llvm-hs-pretty
    megaparsec
    optparse-applicative
    text
  ];

  testHaskellDepends = [
    base
    HUnit
    megaparsec
    tasty
    tasty-hunit
    text
  ];

  preConfigure = "hpack";

  description = "Simple compiler for PA037 course";
  license = stdenv.lib.licenses.mit;
}
