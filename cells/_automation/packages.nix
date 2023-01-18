{
  inputs,
  cell,
}: let
  # explicitly use latest index to avoid warning
  inherit (inputs.cells.prelude.library) project haskell-nix last-index-state;
in {
  fourmolu = haskell-nix.tool project.args.compiler-nix-name "fourmolu" {
    version = "latest";
    index-state = last-index-state;
  };
}
