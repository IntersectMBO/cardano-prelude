{
  haskell-nix,
  src,
}: let
  inherit (haskell-nix) haskellLib;
in
  haskell-nix.cabalProject' ({
    config,
    pkgs,
    lib,
    ...
  }: {
    # Uncomment for your system you want to see `nix flake show` without build agent for other systems:
    #evalSystem = "x86_64-linux";
    #evalSystem = "x86_64-darwin";
    #evalSystem = "aarch64-darwin";

    name = "cardano-prelude";
    # default-compiler
    compiler-nix-name = lib.mkDefault "ghc925";
    # We clean-up src to avoid rebuild for unrelated changes:
    src = haskellLib.cleanSourceWith {
      inherit src;
      name = "cardano-prelude-src";
      filter = path: type: let
        relPath = lib.removePrefix "${src}/" path;
      in
        # excludes top-level directories not part of cabal project:
        (type
          != "directory"
          || (builtins.match ".*/.*" relPath != null)
          || (!(lib.elem relPath [
              "cells"
            ])
            && !(lib.hasPrefix "." relPath)))
        # exclude ".gitignore" files
        && !(lib.hasSuffix ".gitignore" relPath)
        # only keep cabal.project from files at root:
        && (type == "directory" || builtins.match ".*/.*" relPath != null || (relPath == "cabal.project"));
    };
    shell = {
      # not used
      withHoogle = false;
      # Skip cross compilers for the shell
      crossPlatforms = p: [];
    };
    modules = let
      # deduce package names from the cabal project to avoid hard-coding them:
      projectPackageNames =
        builtins.attrNames (haskellLib.selectProjectPackages
          (haskell-nix.cabalProject' (builtins.removeAttrs config ["modules"])).hsPkgs);
    in [
      {
        # compile all local project packages with -Werror
        packages =
          lib.genAttrs projectPackageNames
          (name: {configureFlags = ["--ghc-option=-Werror"];});
      }
    ];
  })
