{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.std) std presets;
in {
  treefmt = presets.nixago.treefmt {
    configData.formatter = {
      prettier = {
        # TODO: remove excludes
        excludes = ["**.md" "**.toml"];
      };
      haskell = {
        command = "fourmolu";
        options = [
          "--mode"
          "inplace"
          "--check-idempotence"
        ];
        # TODO: activate haskell formatter:
        #includes = ["*.hs"];
      };
    };
    packages = [cell.packages.fourmolu];
  };

  editorconfig = presets.nixago.editorconfig {
    configData = {
      "*.hs" = {
        indent_style = "space";
        indent_size = 2;
        trim_trailing_whitespace = "true";
        insert_final_newline = "true";
        charset = "utf-8";
        end_of_line = "lf";
      };
    };
  };

  conform = presets.nixago.conform;

  lefthook = presets.nixago.lefthook {
    # XXX: skip checking conformity of commit msg:
    configData.commit-msg.commands.conform.run = "true";
  };
}
