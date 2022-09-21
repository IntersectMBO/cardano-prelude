{ haskell-nix }:

with haskell-nix.haskellLib;
{

  inherit
    selectProjectPackages
    collectComponents'
    collectChecks';

  inherit (extra)
    recRecurseIntoAttrs;

}
