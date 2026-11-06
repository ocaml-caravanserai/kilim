{
  description = "kilim: OCaml 5.5 development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # The fork bikallem/windtrap, as in caravan and weft.
    windtrap = {
      url = "github:bikallem/windtrap";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      windtrap,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope (
              final: prev: {
                # nixpkgs ships an older windtrap and no ppx_windtrap. The two
                # packages of the windtrap flake use the OCaml of this scope.
                windtrap = final.callPackage windtrap.lib.windtrap { };
                ppx_windtrap = final.callPackage windtrap.lib.ppx_windtrap { };
              }
            );
          in
          f { inherit pkgs ocamlPackages; }
        );
    in
    {
      devShells = forAllSystems (
        { pkgs, ocamlPackages }:
        {
          default = pkgs.mkShell {
            packages = with ocamlPackages; [
              # Toolchain
              ocaml
              dune_3
              findlib

              # The two browser compilers. wasm_of_ocaml needs binaryen.
              js_of_ocaml-compiler
              js_of_ocaml
              wasm_of_ocaml-compiler
              pkgs.binaryen

              # Browser bindings
              brr

              # Tests: windtrap, and node to run the browser builds.
              ocamlPackages.windtrap
              ocamlPackages.ppx_windtrap
              pkgs.nodejs

              # Dev tools
              ocaml-lsp
              utop
              ocamlformat
              odoc
            ];
          };
        }
      );

      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);
    };
}
