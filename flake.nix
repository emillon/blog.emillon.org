{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=b3a8a4ee4a7eaa83f38a7076a29dc156663d4608";
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
