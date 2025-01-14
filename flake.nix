{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    # non-flake nix compatibility
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = inputs:
    let
      profiling = false;
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        # not supported on ci.iog.io right now
        #"aarch64-linux"
        "aarch64-darwin"
       ]; in
    let flake = inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = [inputs.haskellNix.overlay] ++ builtins.attrValues inputs.iohkNix.overlays;
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        # ... and construct a flake from the cabal.project file.
        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        perSystemFlake = (nixpkgs.haskell-nix.cabalProject' ({pkgs, lib, config, ...}: {
          src = ./.;
          name = "cardano-prelude";
          compiler-nix-name = "ghc92";
          flake = {
            variants = {
              ghc810.compiler-nix-name = lib.mkForce "ghc810";
              ghc96.compiler-nix-name = lib.mkForce "ghc96";
              ghc98.compiler-nix-name = lib.mkForce "ghc98";
              ghc910.compiler-nix-name = lib.mkForce "ghc910";
            };

            # we also want cross compilation to windows.
            crossPlatforms = p:
              lib.optional (system == "x86_64-linux" && builtins.elem config.compiler-nix-name ["ghc8107" "ghc928"]) p.mingwW64;
          };

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
          };
        })).flake {};
      in perSystemFlake
    ); in let pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
    # This is not ideal, it means this flake will not evaluate properly on
    # anything but linux :-/ However, CI requires a top-level required job,
    # for now. And not one per system.
    in pkgs.lib.recursiveUpdate (removeAttrs flake ["checks"]) {
      # add a required job, that's basically all hydraJobs.
      hydraJobs = pkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
        { ciJobs = flake.hydraJobs; };

      checks = let
        # https://github.com/numtide/flake-utils/issues/121#issuecomment-2589899217
        recurseIntoDeepAttrs = attrs:
          pkgs.lib.recurseIntoAttrs (pkgs.lib.mapAttrs (_: v:
            if builtins.typeOf v == "set" && !pkgs.lib.isDerivation v
            then recurseIntoDeepAttrs v
            else v
          ) attrs);
      in pkgs.lib.genAttrs supportedSystems (system:
        inputs.flake-utils.lib.flattenTree (recurseIntoDeepAttrs flake.hydraJobs.${system})
        // pkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates { ciJobs = flake.hydraJobs.${system}; }
      );
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      # drop this, once we stop needing it; when we have stable aarch64-darwin
      # builds
      "https://cache.zw3rk.com"
      "https://cache.garnix.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
    allow-import-from-derivation = true;
  };
}
