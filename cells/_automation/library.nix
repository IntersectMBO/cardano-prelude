{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) lib;
in {
  ciProject = inputs.cells.prelude.library.project.appendModule ({pkgs, ...}: {
    # Cross compilation support:
    crossPlatforms = p:
      lib.optionals pkgs.stdenv.hostPlatform.isLinux (
        [p.musl64]
        ++ lib.optional pkgs.stdenv.hostPlatform.isx86_64 p.mingwW64
      );
    # extra-compilers
    flake.variants = lib.genAttrs ["ghc8107"] (x: {compiler-nix-name = x;});
  });
}
