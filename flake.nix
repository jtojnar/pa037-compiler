{
  description = "Simple compiler for PA037 course";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-compat, nixpkgs, utils }: utils.lib.eachDefaultSystem (system: let
    pkgs = nixpkgs.legacyPackages.${system};

    haskellPackages = pkgs.haskellPackages.override {
      overrides = final: prev: {
        compiler = final.callPackage ./compiler.nix {};

        # Incorrectly marked as broken
        llvm-hs = pkgs.haskell.lib.unmarkBroken prev.llvm-hs;
        llvm-hs-pretty = pkgs.haskell.lib.unmarkBroken prev.llvm-hs-pretty;
      };
    };

  in {
    devShell = self.packages.${system}.compiler.env.overrideAttrs (attrs: {
      nativeBuildInputs = attrs.nativeBuildInputs ++ [
        pkgs.cabal-install
      ];
    });

    packages.compiler = haskellPackages.compiler;

    defaultPackage = self.packages.${system}.compiler;

    apps.compiler = utils.lib.mkApp { drv = self.packages.${system}.compiler; };

    defaultApp = self.apps.${system}.compiler;
  });
}
