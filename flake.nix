{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.follows = "karamel/flake-utils";
    karamel.url = "github:FStarLang/karamel";
    karamel.inputs.nixpkgs.follows = "nixpkgs";

    charon.url = "github:AeneasVerif/charon";
    charon.inputs.nixpkgs.follows = "nixpkgs";
    charon.inputs.nixpkgs-ocaml.follows = "nixpkgs";
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
      fstar = inputs.karamel.inputs.fstar.packages.${system}.default;
      krml = karamel.passthru.lib;

      charon-packages = inputs.charon.packages.${system};
      charon-ml = charon-packages.charon-ml;
      charon = charon-packages.default;

      package =
        { ocamlPackages
        , removeReferencesTo
        , clangStdenv
        , symlinks
        , version
        , which
        , z3
        , gnugrep
        , charon-ml
        , krml
        , symlinkJoin
        , clang
        }:
        let
          eurydice = ocamlPackages.buildDunePackage {
            pname = "eurydice";
            inherit version;

            src = ./.;

            nativeBuildInputs = [ gnugrep ] ++ (with ocamlPackages; [ menhir ]);

            propagatedBuildInputs = [ krml charon-ml ocamlPackages.terminal ocamlPackages.yaml ] ++ (with ocamlPackages; [ menhirLib ]);

            passthru = {
              tests = clangStdenv.mkDerivation {
                name = "tests";
                src = ./.;
                KRML_HOME = karamel;
                FSTAR_HOME = "dummy";
                EURYDICE = "${eurydice}/bin/eurydice";
                buildInputs = [ eurydice ];
                buildPhase = ''
                  shopt -s globstar
                  export CHARON="${charon}/bin/charon"

                  # setup CHARON_HOME: it is expected to be writtable, hence the `cp --no-preserve`
                  cp --no-preserve=mode,ownership -rf ${inputs.charon.sourceInfo.outPath} ./charon
                  export CHARON_HOME=./charon

                  # Move the committed test outputs out of the way
                  mv out out-comitted

                  # Run the tests
                  make -o all test

                  # Clean generated files that we don't want to compare.
                  rm out/**/a.out

                  # Check that there are no differences between the generated
                  # outputs and the committed outputs.
                  if diff -rq out-comitted out; then
                    echo "Ok: the regenerated files are the same as the checked out files"
                  else
                    echo "Error: the regenerated files differ from the checked out files"
                    diff -ru out-comitted out
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
          inherit charon-ml krml;
          version = self.rev or "dirty";
        };
        inherit charon;
      };
      checks.default = packages.default.tests;
      devShells.ci = pkgs.mkShell { packages = [ pkgs.jq ]; };
      devShells.default = (pkgs.mkShell.override { stdenv = pkgs.clangStdenv; }) {
        OCAMLRUNPARAM = "b"; # Get backtrace on exception
        packages = [
          pkgs.clang-tools_18 # For clang-format
          pkgs.ocamlPackages.ocaml
          pkgs.ocamlPackages.ocamlformat_0_27_0
          pkgs.ocamlPackages.menhir
          # ocaml-lsp's version must match the ocaml version used. Pinning
          # this here to save me a headache.
          pkgs.ocamlPackages.ocaml-lsp
        ];
        buildInputs = [ charon.buildInputs ];
        nativeBuildInputs = [ charon.nativeBuildInputs fstar pkgs.clang ];

        inputsFrom = [
          self.packages.${system}.default
          charon-ml
        ];
      };
    });
}
