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

    # Work around replaces `:` with `__` in attribute names
    fixNames = x: __listToAttrs (map (
      name: let v = x.${name}; in
        { name = __replaceStrings [":"] ["__"] name;
        value = if __isAttrs v && !(v.type or null == "derivation")
          then fixNames v else v; }) (__attrNames x));

    hydraJobTasks = mkHydraJobTasks (flakeOutputTasks ["ciJobs" system]
      # TODO put this back to `self` if we get `:` working
      { outputs.ciJobs.${system} = fixNames self.outputs.ciJobs.${system}; });

    ciTasks = taskSequence "ci/" hydraJobTasks (__attrNames hydraJobTasks);
  in
    ciTasks // {
      "ci" = {lib, ...}: {
        imports = [common];
        after = __attrNames ciTasks;
      };
    };

  actions = {
    "cardano-prelude/ci" = {
      task = "ci";
      io = ''
        let github = {
          #input: "${ciInputName}"
          #repo: "input-output-hk/cardano-prelude"
        }
        
        #lib.merge
        #ios: [
          #lib.io.github_push & github,
          #lib.io.github_pr   & github,
        ]
      '';
    };
  };
}
