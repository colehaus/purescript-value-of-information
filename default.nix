let
  extras = import ./extras.nix;
  pkgs = extras.pinnedPkgs {
    specFile = ./nixpkgs.json;
    opts = {};
  };
in
  extras.callPurescript2nix {
    inherit pkgs;
    name = "purescript-value-of-information";
    src = ./.;
    executable = false;
  }
