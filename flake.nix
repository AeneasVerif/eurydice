{
  inputs = {
    karamel.url = "github:FStarLang/karamel";
    flake-utils.follows = "karamel/flake-utils";
    nixpkgs.follows = "karamel/nixpkgs";

    charon.url = "github:AeneasVerif/charon";
    charon.inputs.nixpkgs.follows = "nixpkgs";
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

      karamel = inputs.karamel.packages.${system}.default;
      fstar = inputs.karamel.inputs.fstar.packages.${system}.default;
      krml = karamel.passthru.lib;

      charon-packages = inputs.charon.packages.${system};
      charon-ml = charon-packages.charon-ml;
      charon = charon-packages.default;

      package =
        { ocamlPackages
        , removeReferencesTo
        , stdenv
        , symlinks
        , version
        , which
        , z3
        , gnugrep
        , charon-ml
        , krml
        , symlinkJoin
        , clang
        ,
        }:
        let
          eurydice = ocamlPackages.buildDunePackage {
            pname = "eurydice";
            inherit version;

            src = ./.;

            nativeBuildInputs = [ gnugrep ];

            propagatedBuildInputs = [ krml charon-ml ocamlPackages.terminal ocamlPackages.yaml ];

            passthru = {
              tests = stdenv.mkDerivation {
                name = "tests";
                src = ./.;
                KRML_HOME = karamel;
                FSTAR_HOME = "dummy";
                EURYDICE = "${eurydice}/bin/eurydice";
                buildInputs = [ charon.buildInputs eurydice ];
                nativeBuildInputs = [ charon.nativeBuildInputs clang ];
                buildPhase = ''
                  export CHARON="${charon}/bin/charon"

                  # setup CHARON_HOME: it is expected to be writtable, hence the `cp --no-preserve`
                  cp --no-preserve=mode,ownership -rf ${inputs.charon.sourceInfo.outPath} ./charon
                  export CHARON_HOME=./charon

                  make -o all test
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
      devShells.default = pkgs.mkShell {
        packages = [
          pkgs.clang-tools # For clang-format
          pkgs.ocamlPackages.ocaml
          pkgs.ocamlPackages.ocamlformat
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
