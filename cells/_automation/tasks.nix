{
  inputs,
  cell,
}: let
  common = {config, ...}: {
    preset = {
      nix.enable = true;

      github.ci = {
        enable = config.actionRun.facts != {};
        repository = "input-output-hk/cardano-prelude";
        remote = config.preset.github.lib.readRepository inputs.cells._cloud.library.actionCiInputName null;
        revision = config.preset.github.lib.readRevision inputs.cells._cloud.library.actionCiInputName null;
      };
    };

    nomad.driver = "exec";
    memory = 1024 * 16;
    nomad.resources.cpu = 10000;
  };

  mkJobsTask = jobsAttrs: {
    config,
    lib,
    ...
  }: {
    imports = [common];

    command.text = config.preset.github.status.lib.reportBulk {
      bulk.text = ''
        nix eval .#tullia --apply __attrNames --json |
        nix-systems -i |
        jq 'with_entries(select(.value))' # filter out systems that we cannot build for
      '';
      each.text = ''nix build -L .#packages."$1".${jobsAttrs}'';
      skippedDescription = lib.escapeShellArg "No nix builder available for this system";
    };
    env.NIX_CONFIG = ''
      # bigger timeouts (900 by default on cicero) needed for some derivations (eg. hlint on darwin)
      max-silent-time = 1800
    '';
  };

  tasks = {
    "ci/required" = mkJobsTask "required";
    "ci/nonrequired" = mkJobsTask "nonrequired";
  };
in
  tasks
  // {
    ci = {lib, ...}: {
      imports = [common];
      after = lib.attrNames tasks;
    };
  }
