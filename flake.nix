{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=477a7db1cc3f5e447777a643feb522f9f0bdb535";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        packageName = "blog";
        newDeps = with pkgs.ocamlPackages; [ base cmarkit cmdliner ptime stdio tyxml uri yaml ] ;
        blogEngine = pkgs.haskellPackages.callCabal2nix "blog" self rec { };
        blogEngineNew = pkgs.ocamlPackages.buildDunePackage {
          version = "n/a";
          src = ./.;
          pname = "blog";
          propagatedBuildInputs = newDeps;
        };
        haskellPackages = pkgs.haskellPackages;
      in {
        packages.new = blogEngineNew;
        devShells.new = pkgs.mkShell {
          buildInputs = newDeps
            ++ (with pkgs.ocamlPackages; [ merlin ocamlformat_0_26_1 ]);
          inputsFrom = [ blogEngineNew ];
        };
        packages.default = blogEngine;
      });
}
