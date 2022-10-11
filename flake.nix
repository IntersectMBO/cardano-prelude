{
  # This is a template created by `hix init`
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    tullia = {
       url = "github:input-output-hk/tullia/escape";
       inputs.nixpkgs.follows = "nixpkgs";
     };
     ghc-next-packages = {
      url = "github:input-output-hk/ghc-next-packages?ref=repo";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat, tullia, ghc-next-packages }:
    let
      ci = (builtins.fromTOML (__readFile ./ci.toml)).dimensions;
    in
      flake-utils.lib.eachSystem ci.os (system:
      let
        compilers = builtins.filter
          (x: !__elem "${system}.${x}" ci.disable)
          (ci.compiler);
        overlays = [ haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
                compiler-nix-name = __head compilers;
                inputMap."https://input-output-hk.github.io/ghc-next-packages" = ghc-next-packages;
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {
          variants = pkgs.lib.genAttrs (__tail compilers)
            (x: { compiler-nix-name = pkgs.lib.mkForce x; });
        };
      in flake // {
        legacyPackages = pkgs;
      } // tullia.fromSimple system (import ./nix/tullia.nix self system));

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
