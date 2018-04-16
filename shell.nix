let
  extras = import ./extras.nix;
  pkgs = extras.pinnedPkgs {
    specFile = ./nixpkgs.json;
    opts = {};
  };
in
  extras.purescriptDevEnv { inherit pkgs; }
