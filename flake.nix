{
  inputs = {
    nixpkgs.follows = "charon/nixpkgs";
    flake-utils.follows = "karamel/flake-utils";
    karamel.url = "github:FStarLang/karamel";
    karamel.inputs.nixpkgs.follows = "nixpkgs";

    charon.url = "github:AeneasVerif/charon";
    crane.follows = "charon/crane";

    libcrux.url = "github:cryspen/libcrux";
    googletest.follows = "libcrux/googletest";
    benchmark.follows = "libcrux/benchmark";
    json.follows = "libcrux/json";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , ...
    } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };

      karamel = inputs.karamel.packages.${system}.default.override {
        ocamlPackages = pkgs.ocamlPackages;
      };
      krml = karamel.passthru.lib;

      charon-packages = inputs.charon.packages.${system};
      charon-ml = charon-packages.charon-ml.override {
        ocamlPackages = pkgs.ocamlPackages;
      };
      charon = charon-packages.default;
      craneLib = (inputs.crane.mkLib pkgs).overrideToolchain charon-packages.rustToolchain;

      package =
        { ocamlPackages
        , removeReferencesTo
        , clangStdenv
        , symlinks
        , version
        , which
        , z3
        , cmake
        , git
        , mold-wrapped
        , ninja
        , gnugrep
        , charon-ml
        , krml
        , symlinkJoin
        , clang
        , craneLib
        }:
        let
          eurydice = ocamlPackages.buildDunePackage {
            pname = "eurydice";
            inherit version;

            src = ./.;

            nativeBuildInputs = [ gnugrep ] ++ (with ocamlPackages; [ menhir ]);

            propagatedBuildInputs = [ krml charon-ml ocamlPackages.terminal ocamlPackages.yaml ] ++ (with ocamlPackages; [ menhirLib ]);

            passthru = {
              tests = clangStdenv.mkDerivation rec {
                name = "tests";
                src = ./.;
                IN_CI = 1; # Tell the `check-dependency` script to not check for charon/karamel commit hashes.
                KRML_HOME = karamel.src;
                EURYDICE = "${eurydice}/bin/eurydice";
                CHARON = "${charon}/bin/charon";
                CMAKE_FLAGS = [
                  "-DFETCHCONTENT_SOURCE_DIR_GOOGLETEST=${inputs.googletest}"
                  "-DFETCHCONTENT_SOURCE_DIR_BENCHMARK=${inputs.benchmark}"
                  "-DFETCHCONTENT_SOURCE_DIR_JSON=${inputs.json}"
                  "-DFETCHCONTENT_FULLY_DISCONNECTED=ON"
                ];

                # Pre-build the cargo dependencies; required for `cargo` to
                # work inside nix since nix builders don't have network
                # access.
                cargoVendorDir = craneLib.vendorCargoDeps { cargoLock = ./libcrux-Cargo.lock; };
                cargoArtifacts = craneLib.buildDepsOnly {
                  src = inputs.libcrux.sourceInfo.outPath;
                  inherit cargoVendorDir;
                  # Only build the libcrux-ml-kem package.
                  cargoExtraArgs = "-p libcrux-ml-kem";
                  # Run `cargo check` too, to make sure we include the dev-dependencies.
                  doCheck = true;
                };

                nativeBuildInputs = [
                  cmake
                  git
                  mold-wrapped
                  ninja
                  # Crane hooks that will use the
                  # `cargoArtifacts`/`cargoVendorDir` to tell cargo where to
                  # find its dependencies.
                  craneLib.configureCargoCommonVarsHook
                  craneLib.configureCargoVendoredDepsHook
                  craneLib.inheritCargoArtifactsHook
                  pkgs.zstd # For the inheritCargoArtifactsHook hooks.
                ];
                buildInputs = [ eurydice ];

                dontUseCmakeConfigure = true;
                buildPhase = ''
                  # Prepare the libcrux directory.
                  cp --no-preserve=mode,ownership -rf ${inputs.libcrux.sourceInfo.outPath} libcrux
                  # Symlink karamel so we find its headers.
                  ln -s $KRML_HOME karamel

                  # Run the tests
                  make -o all test

                  # Remove generated files that we don't want to compare.
                  shopt -s globstar
                  rm -f out/**/a.out
                  # Check that there are no differences between the generated
                  # outputs and the committed outputs.
                  if diff -rq "${./out}" out; then
                    echo "Ok: the regenerated files are the same as the checked out files"
                  else
                    echo "Error: the regenerated files differ from the checked out files"
                    diff -ru "${./out}" out
                    exit 1
                  fi
                '';
                installPhase = ''touch $out'';
              };
            };
          };
        in
        eurydice;
    in
    rec {
      packages = {
        default = pkgs.callPackage package {
          inherit charon-ml krml craneLib;
          version = self.rev or "dirty";
        };
        inherit charon karamel;
      };
      checks = {
        default = packages.default.tests;
        format = pkgs.runCommand "format-check"
          {
            src = ./.;
            nativeBuildInputs = [
              pkgs.bash
              pkgs.gnumake
              pkgs.llvmPackages_18.clang-tools # For clang-format
              pkgs.ocamlPackages.ocaml
              pkgs.ocamlPackages.ocamlformat_0_27_0
              pkgs.ocamlPackages.dune_3
            ];
          } ''
          cp -r $src src
          chmod u+w src
          cd src
          bash ./scripts/format.sh check
          touch $out
        '';
      };
      devShells.ci = pkgs.mkShell { packages = [ pkgs.jq ]; };
      devShells.default = (pkgs.mkShell.override { stdenv = pkgs.clangStdenv; }) {
        OCAMLRUNPARAM = "b"; # Get backtrace on exception
        packages = [
          pkgs.jq
          pkgs.llvmPackages_18.clang-tools # For clang-format
          pkgs.ocamlPackages.ocaml
          pkgs.ocamlPackages.ocamlformat_0_27_0
          pkgs.ocamlPackages.menhir
          # ocaml-lsp's version must match the ocaml version used. Pinning
          # this here to save me a headache.
          pkgs.ocamlPackages.ocaml-lsp
          pkgs.rustup
        ];
        buildInputs = [ charon.buildInputs ];
        nativeBuildInputs = [ charon.nativeBuildInputs pkgs.clang ];

        inputsFrom = [
          packages.default
          packages.default.tests
          charon-ml
        ];
      };
    });
}
