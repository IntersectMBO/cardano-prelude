let
  commonLib = import ./lib.nix;
  default = import ./default.nix {};
  # Path of nix-tools jobs that we want to evict from release.nix:
  disabled = [
    # FIXME: those tests freeze on darwin hydra agents:
  ];
in
{ cardano-prelude ? { outPath = ./.; rev = "acdef"; }, ... }@args:
commonLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(commonLib.nix-tools.release-nix {
  package-set-path = ./nix/nix-tools.nix;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are interested in building on CI via nix-tools.
  packages = [ "cardano-prelude" ];

  # The set of jobs we consider crutial for each CI run.
  # if a single one of these fails, the build will be marked
  # as failed.
  #
  # The names can be looked up on hydra when in doubt.
  #
  # custom jobs will follow their name as set forth in
  # other-packages.
  #
  # nix-tools packages are prefixed with `nix-tools` and
  # follow the following naming convention:
  #
  #   namespace                      optional cross compilation prefix                 build machine
  #   .-------.                              .-----------------.                .--------------------------.
  #   nix-tools.{libs,exes,tests,benchmarks}.{x86_64-pc-mingw-,}$pkg.$component.{x86_64-linux,x86_64-darwin}
  #             '--------------------------'                    '-------------'
  #                 component type                          cabal pkg and component*
  #
  # * note that for `libs`, $component is empty, as cabal only
  #   provides a single library for packages right now.
  # * note that for `exes`, $component is also empty, because it
  #   it provides all exes under a single result directory.
  #   To  specify a single executable component to build, use
  #   `cexes` as component type.
  #

  # The required jobs that must pass for ci not to fail:
  required-name = "required";
  required-targets = jobs: [
    # targets are specified using above nomenclature:
    jobs.nix-tools.libs.cardano-prelude.x86_64-linux
  ];
} (builtins.removeAttrs args ["cardano-prelude"]))
