{ mkDerivation, array, base, containers, hspec, hspec-discover
, multiset, QuickCheck, stdenv
}:
mkDerivation {
  pname = "shokunin20";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ array base ];
  executableHaskellDepends = [ array base ];
  testHaskellDepends = [ base containers hspec multiset QuickCheck ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
