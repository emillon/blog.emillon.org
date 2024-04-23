{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };
  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        blogEngine = pkgs.haskellPackages.callCabal2nix "blog" self rec { };
        contentSrc = nix-filter {
          root = ./.;
          include = [ "posts" "static" "templates" ];
        };
        newDeps = with pkgs.ocamlPackages; [ cmdliner tyxml yaml ] ;
        blogEngineNew = pkgs.ocamlPackages.buildDunePackage {
          version = "n/a";
          src = ./.;
          pname = "blog";
          propagatedBuildInputs = newDeps;
        };
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "blog-contents";
          nativeBuildInputs = [ blogEngine ];
          src = contentSrc;
          buildPhase = "LC_ALL=C.UTF-8 blog build";
          installPhase = "cp -r _site/. $out/";
        };
        packages.new = pkgs.stdenv.mkDerivation {
          name = "blog-contents-new";
          nativeBuildInputs = [ blogEngineNew ];
          src = contentSrc;
          buildPhase = "blog -i . -o output";
          installPhase = "cp -r output/. $out/";
        };
        devShells.new = pkgs.mkShell {
          buildInputs = newDeps
            ++ (with pkgs.ocamlPackages; [ ocaml-lsp ocamlformat_0_26_1 ]);
          inputsFrom = [ blogEngineNew ];
        };
      });
}
