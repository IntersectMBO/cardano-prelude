{
  inputs,
  cell,
}: let
  # explicitly use latest index to avoid warning
  inherit (cell.library) haskell-nix last-index-state;
in {
  mkDev = project: _: let
    inherit (project.args) compiler-nix-name;
  in {
    imports = [
      (haskell-nix.haskellLib.devshellFor project.shell)
    ];

    commands = [
      {
        package = haskell-nix.cabal-install.${compiler-nix-name};
        name = "cabal";
        category = "development";
      }
      {
        package = haskell-nix.tool compiler-nix-name "ghcid" {
          version = "latest";
          index-state = last-index-state;
        };
        name = "ghcid";
        category = "development";
      }
      {
        package = haskell-nix.tool compiler-nix-name "hlint" {
          version = {ghc8107 = "3.4.1";}.${compiler-nix-name} or "latest";
          index-state = last-index-state;
        };
        name = "hlint";
        category = "development";
      }
      {
        package = haskell-nix.tool compiler-nix-name "weeder" {
          version = {ghc8107 = "2.2.0";}.${compiler-nix-name} or "latest";
          index-state = last-index-state;
        };
        name = "weeder";
        category = "development";
      }
    ];
  };
}
