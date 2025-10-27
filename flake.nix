{
  description = "Exponential sum of the day";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in
      {
        packages.esotd = haskellPackages.callCabal2nix "esotd" self { };

        defaultPackage = self.packages.${system}.esotd;

        apps.esotd = {
          type = "app";
          program = "${self.packages.${system}.esotd}/bin/esotd";
        };

        defaultApp = self.apps.${system}.esotd;

        checks.default = self.packages.${system}.esotd;
      });
}
