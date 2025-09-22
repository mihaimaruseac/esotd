{
  description = "Exponential sum of the day";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        packageName = "esotd";
      in
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self rec {
          # Dependency overrides
        };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            cabal-install
            diagrams
            diagrams-lib
            diagrams-rasterific
            palette
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };

        #apps.esotd = {
        #  type = "app";
        #  program = "${self.packages.${system}.esotd}/bin/esotd";
        #};

        #defaultApp = self.apps.${system}.esotd;

        #devShells = {
        #  default = haskellPackages.shellFor {
        #    packages = p: [
        #      esotd
        #      pkgs.doctest
        #      pkgs.haskellPackages.diagrams
        #      pkgs.haskellPackages.diagrams-rasterific
        #      pkgs.haskellPackages.palette
        #    ];
        #    buildInputs = [
        #      pkgs.cabal-install
        #      pkgs.doctest
        #      pkgs.haskellPackages.diagrams
        #      pkgs.haskellPackages.diagrams-rasterific
        #      pkgs.haskellPackages.palette
        #    ];
        #  };
        #};

        #checks.default = self.packages.${system}.esotd;
      });
}
