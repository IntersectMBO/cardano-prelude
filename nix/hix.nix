{config, pkgs, ...}: {
  name = "cardano-prelude";

  modules = [{
    packages.cardano-prelude.configureFlags = [ "--ghc-option=-Werror" ];
  }];

  shell = {
    tools = {
      cabal = "latest";
      ghcid = "latest";
      hlint = { ghc8107 = "3.4.1"; }.${config.compiler-nix-name} or "latest";
      weeder = { ghc8107 = "2.2.0"; }.${config.compiler-nix-name} or "latest";
      # haskell-language-server = "latest";
    };

    buildInputs = [
      pkgs.buildPackages.gitAndTools.git
      pkgs.buildPackages.pkgconfig
      pkgs.buildPackages.sqlite-interactive
    ];

    # Skip cross compilers for the shell
    crossPlatforms = p: [];

    exactDeps = false;
  };

  # Cross compilation support:
  crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
     p.mingwW64
  ] ++ pkgs.lib.optionals (config.compiler-nix-name == "ghc8107") [
     p.ghcjs
  ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
     p.musl64
  ]);
}
