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
          buildInputs = [
            pkgs.cabal-install
            pkgs.zlib.dev
            pkgs.haskellPackages.diagrams
            pkgs.haskellPackages.diagrams-rasterific
            pkgs.haskellPackages.palette
          ];
          inputsFrom = builtins.attrValues self.packages.${system};

          shellHook = ''
            export PS1="[\[\033[01;32m\]nix-shell\[\033[00m\]:\W] \[\033[01;32m\]λ\[\033[00m\] "
          '';
        };
      });
}
