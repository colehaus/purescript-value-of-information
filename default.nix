let
  pkgs = import ./pinned-pkgs.nix ./nixpkgs.json {};
  name = "purescript-value-of-information";
  src = ./.;
  bowerDeps = pkgs.callPackage ./callBower2nix.nix {
    inherit name;
    src = "${src}/bower.json";
  };
  npmDeps = pkgs.callPackage ./callNode2nix.nix {
    inherit name;
    src = "${src}/package.json";
  };
in
  pkgs.stdenv.mkDerivation {
    doCheck = true;
    setSourceRoot = ''
      sourceRoot=$(pwd)
    '';
    srcs = [ "${src}/src" "${src}/test" "${src}/bower.json" ];
    inherit name;
    nativeBuildInputs = [
      pkgs.nodePackages.pulp
      pkgs.purescript

      npmDeps.shell.nodeDependencies
      pkgs.nodejs
    ];
    phases = [ "unpackPhase" "configurePhase" "buildPhase" "checkPhase" "fixupPhase" ];
    preUnpack = ''
      unpackCmdHooks+=(_cpFile)
      _cpFile() {
        local fn="$1"
        cp -p --reflink=auto -- "$fn" "$(stripHash "$fn")"
      }
    '';
    configurePhase = ''
      mkdir -p bower_components
      for hash in "${bowerDeps}"/packages/*; do
        for version in "$hash"/*; do
          echo $hash
          echo $version
          cp -r "$version" bower_components/purescript-$(basename "$hash")
        done
      done
    '';
    buildPhase = ''
      pulp build --build-path "$out"
    '';
    checkPhase = ''
      pulp test
    '';
  }
