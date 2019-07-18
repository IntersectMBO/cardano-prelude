with import ../lib.nix;
with pkgs;

let
  stack-pkgs = import ./.stack.nix;
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

in haskell.lib.buildStackProject {
  name = "cardano-prelude-env";
  ghc = haskell.packages.${compiler}.ghc;
}
