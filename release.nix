############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-prelude ? { outPath = ./.; rev = "abcdef"; }
, pr ? null
}:

let
  inherit (import ./nix/flake-compat.nix {
    src = cardano-prelude;
  }) defaultNix;

  jobs = if (pr == null) then defaultNix.hydraJobs else defaultNix.hydraJobsPr;

in
jobs
