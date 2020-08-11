let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          shokunin20 =
            haskellPackagesNew.callPackage ./shokunin20.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { shokunin20 = pkgs.haskellPackages.shokunin20;
  }
