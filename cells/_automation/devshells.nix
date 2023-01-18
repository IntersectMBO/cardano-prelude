{
  cell,
  inputs,
}: let
  inherit (inputs.nixpkgs) lib;
  inherit (inputs.std) std presets;
  inherit (inputs.std.lib) dev;
  inherit (inputs.cells) prelude;
  inherit (cell.library) ciProject;

  mkDevShell = variant: project:
    dev.mkShell {
      name = "Cardano Prelude${lib.optionalString (variant != "") " - ${variant}"}";
      imports = [
        std.devshellProfiles.default
        (prelude.devshellProfiles.mkDev project)
      ];
      nixago = lib.attrValues cell.nixago;
      commands = [
        {
          package = inputs.tullia.tullia.apps.tullia;
          name = "tullia";
          category = "ci";
        }
        {
          package = inputs.tullia.tullia.apps.nix-systems;
          name = "nix-systems";
          category = "ci";
        }
      ];
    };
in
  lib.mapAttrs mkDevShell ciProject.projectVariants
  // {
    default = mkDevShell "" ciProject;
  }
