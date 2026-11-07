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
        let
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
        in
        {
          default = pkgs.mkShell { inherit packages; };
        }
        # The shell of the Firefox and WebKit tests (`nix develop .#browsers`,
        # Linux only): the default shell, playwright-core and its two browsers.
        # npm needs the network at each install, and its browsers use the
        # libraries of the system. nixpkgs builds playwright-core and the
        # browsers from one browsers.json, thus they always match. An update
        # of flake.lock updates the three, and the CI job `browsers` tests the
        # update. The default shell does not download them (about 385 MiB).
        # With flake.lock of this commit: playwright-core 1.64.1, Firefox
        # 157.0 and WebKit 27.2. WebKit needs the EGL driver of the nix mesa:
        # with no driver, or with the mesa of the system, it does not start
        # ("Could not create EGL display").
        // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          browsers = pkgs.mkShell {
            packages = packages ++ [ pkgs.playwright-driver ];
            KILIM_PLAYWRIGHT = "${pkgs.playwright-driver}";
            PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers.override {
              withChromium = false;
              withChromiumHeadlessShell = false;
              withFfmpeg = false;
            };
            __EGL_VENDOR_LIBRARY_FILENAMES = "${pkgs.mesa}/share/glvnd/egl_vendor.d/50_mesa.json";
          };
        }
      );

      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);
    };
}
