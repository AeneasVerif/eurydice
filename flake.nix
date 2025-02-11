{
  inputs = {
    karamel.url = "github:FStarLang/karamel/lucas-nix-aarch64";
    flake-utils.follows = "karamel/flake-utils";
    # Need to use same-ish nixpkgs version as karamel to get a compatible ocaml
    # toolchain
    nixpkgs.follows = "karamel/nixpkgs";
    # Need a recent nixpkgs to get ocamlformat 0.26.2
    recent_nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

    charon.url = "github:AeneasVerif/charon";
    charon.inputs.nixpkgs.follows = "recent_nixpkgs";
    charon.inputs.nixpkgs-ocaml.follows = "nixpkgs";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , recent_nixpkgs
    , ...
    } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      recent_pkgs = import recent_nixpkgs { inherit system; };

      karamel = inputs.karamel.packages.${system}.default;
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

            nativeBuildInputs = [ gnugrep ];

            propagatedBuildInputs = [ krml charon-ml ocamlPackages.terminal ocamlPackages.yaml ];

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
      packages.default = pkgs.callPackage package {
        inherit charon-ml krml;
        version = self.rev or "dirty";
      };
      checks.default = packages.default.tests;
      devShells.ci = pkgs.mkShell { packages = [ pkgs.jq ]; };
      devShells.default = (pkgs.mkShell.override { stdenv = pkgs.clangStdenv; }) {
        OCAMLRUNPARAM = "b"; # Get backtrace on exception
        packages = [
          pkgs.clang-tools # For clang-format
          pkgs.ocamlPackages.ocaml
          recent_pkgs.ocamlPackages.ocamlformat_0_26_2
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
