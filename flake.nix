{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    karamel.url = "github:FStarLang/karamel";
    fstar.url = "github:FStarLang/fstar";

    nixpkgs.follows = "karamel/nixpkgs";

    charon.url = "github:AeneasVerif/charon";
    charon.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = {
    self,
    flake-utils,
    nixpkgs,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};

      karamel = inputs.karamel.packages.${system}.default;
      krml = karamel.passthru.lib;

      charon-packages = inputs.charon.packages.${system};
      charon-ml = charon-packages.charon-ml;
      charon = charon-packages.default;

      fstar = inputs.fstar.packages.${system}.default;

      package = {
        ocamlPackages,
        removeReferencesTo,
        stdenv,
        symlinks,
        version,
        which,
        z3,
        gnugrep,
        charon-ml,
        krml,
        symlinkJoin,
        clang,
      }: let
        eurydice = ocamlPackages.buildDunePackage {
          pname = "eurydice";
          inherit version;

          src = ./.;

          nativeBuildInputs = [gnugrep];

          propagatedBuildInputs = [krml charon-ml ocamlPackages.terminal ocamlPackages.yaml];

          passthru = {
            tests = stdenv.mkDerivation {
              name = "tests";
              src = ./.;
              KRML_HOME = karamel;
              FSTAR_HOME = fstar;
              EURYDICE = "${eurydice}/bin/eurydice";
              buildInputs = [charon.buildInputs eurydice];
              nativeBuildInputs = [charon.nativeBuildInputs fstar clang];
              buildPhase = ''
                # we need to tell `charon` not to use `rustup`
                export CHARON="${charon}/bin/charon --cargo-no-rust-version"

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
    in rec {
      packages.default = pkgs.callPackage package {
        inherit charon-ml krml;
        version = self.rev or "dirty";
      };
      checks.default = packages.default.tests;
    });
}
