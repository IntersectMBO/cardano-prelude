self: system:

let
  ciInputName = "GitHub event";
in rec {
  tasks = let
    inherit (self.inputs.tullia) flakeOutputTasks taskSequence;

    common = {
      config,
      ...
    }: {
      preset = {
        # needed on top-level task to set runtime options
        nix.enable = true;

        github-ci = {
          enable = config.actionRun.facts != {};
          repo = "input-output-hk/cardano-prelude";
          sha = config.preset.github-ci.lib.getRevision ciInputName null;
        };
      };
    };

    mkHydraJobTask = flakeOutputTask: {...}: {
      imports = [common flakeOutputTask];

      # some hydra jobs run NixOS tests
      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 8;
      nomad.resources.cpu = 10000;
    };
    mkHydraJobTasks = __mapAttrs (_: mkHydraJobTask);

    hydraJobTasks   = mkHydraJobTasks (flakeOutputTasks ["ciJobs" system] self);

    ciPushTasks = taskSequence "ci/push/" hydraJobTasks   (__attrNames hydraJobTasks);
  in
    ciPushTasks // {
      "ci/push" = {lib, ...}: {
        imports = [common];
        after = __attrNames ciPushTasks;
      };
    };

  actions = {
    "cardano-prelude/ci/push" = {
      task = "ci/push";
      io = ''
        #lib.io.github_push
        #input: "${ciInputName}"
        #repo: "input-output-hk/cardano-prelude"
      '';
    };
  };
}
