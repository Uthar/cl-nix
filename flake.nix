{
  description = "Nix C++ bindings for Common Lisp";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.devpkgs.url = "/home/kasper/dev";

  outputs = { self, nixpkgs, flake-utils, devpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        dev = devpkgs.packages.${system};
      in
      {
        packages = rec {
          clasp = dev.clasp;
          default = clasp;
        };
      }
    );
}
