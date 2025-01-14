{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=0bc7caf6fffca0847fa2103f9f6b6e2a8f2dcfbf";
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
