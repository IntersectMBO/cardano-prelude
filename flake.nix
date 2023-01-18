{
  # This is a template created by `hix init`
  inputs = {
    nixpkgs = {
      follows = "std/nixpkgs";
    };
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    haskellNix = {
      inputs = {
        hackage.follows = "hackage";
      };
      url = "github:input-output-hk/haskell.nix";
    };
    std = {
      url = "github:divnix/std";
    };
    tullia = {
      url = "github:input-output-hk/tullia";
      inputs = {
        std.follows = "std";
      };
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs = {
    std,
    tullia,
    self,
    ...
  } @ inputs:
    std.growOn {
      inherit inputs;
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      cellsFrom = ./cells;
      cellBlocks = [
        (std.functions "library")
        (std.installables "packages")
        (std.runnables "apps")
        (std.nixago "nixago")
        (std.functions "devshellProfiles")
        (std.devshells "devshells")
        (std.installables "ciJobs")
        (tullia.tasks "tasks")
        (std.functions "actions")
      ];
    }
    # soil layers:
    {
      # nix-cli compat
      packages = std.harvest self [["prelude" "packages"] ["prelude" "library"] ["_automation" "ciJobs"] ["_automation" "packages"]];
      devShells = std.harvest self ["_automation" "devshells"];
    }
    ( # Cicero / tullia CI jobs:
      tullia.fromStd {
        actions = std.harvest self ["_cloud" "actions"];
        tasks = std.harvest self ["_automation" "tasks"];
      }
    );

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
