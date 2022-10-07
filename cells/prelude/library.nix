{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) lib;
  inherit (inputs.haskellNix.legacyPackages) haskell-nix;

  project = import ./haskell.nix {
    inherit haskell-nix;
    src = inputs.self;
  };

  last-index-state = lib.last (builtins.attrNames (import haskell-nix.indexStateHashesPath));
in {
  inherit haskell-nix project last-index-state;
}
