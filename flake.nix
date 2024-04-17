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
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "blog-contents";
          nativeBuildInputs = [ blogEngine ];
          src = nix-filter {
            root = ./.;
            include = [ "posts" "static" "templates" ];
          };
          buildPhase = "LC_ALL=C.UTF-8 blog build";
          installPhase = "cp -r _site/. $out/";
        };
      });
}
