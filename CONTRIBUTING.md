# Contributing

## Prerequisites

Nix and direnv (recommended) are used to provide all necessary developments tools: cabal, GHC, formatters, etc.

### Nix

Install [Nix](https://nixos.org/nix/) (recommended).
Then [activate Flakes support for Nix](https://nixos.wiki/wiki/Flakes) (mandatory).

The nix setup ([`flake.nix`](./flake.nix) and everything under [`cells`](./cells/)) in this repo use the [Standard framework](https://github.com/divnix/std) (`std`), please refer to its [documentation](https://std.divnix.com/) for general maintenance.

We also use [Haskell.nix](https://github.com/input-output-hk/haskell.nix) for building haskell packages with nix. See [`haskell.nix`](./cells/prelude/haskell.nix) configuration file and the [Haskell.nix documentation](https://input-output-hk.github.io/haskell.nix/) for maintenance.

### Development environment: direnv or nix develop.

Development environments are provided for the following compilers:

- GHC 9.2.x (default)
- GHC 8.10.x

Upon entering a development environment for the first time, `nix` may ask you to activate the necessary option and the IOG nix cache (see `nixConfig` attribute in [`flake.nix`](./flake.nix)).

#### Direnv

[direnv](https://direnv.net/) is recommended, [install it for your OS and hook it to your shell](https://direnv.net/docs/installation.html).

Then

```console
$ direnv allow
```

##### Use alternative compiler configuration

Check available alternatives with:

```console
$ std list | grep -Eo '//_automation/devshells/.*:enter'
//_automation/devshells/default:enter
//_automation/devshells/ghc8107:enter
```

Then you can enter one of the alternative shell with, eg.:

```console
$ std //_automation/devshells/ghc8107:enter
```

(here the shell will provide GHC 8.10.7)

or use it permanently:

```console
$ echo DEVSHELL_TARGET=ghc8107 >> .envrc.local
$ direnv reload
```

#### Nix develop

use `nix develop` or `nix develop .#ghc8107` to enter a development shell.

## Development workflows

#### Available tools

Available tools (`cabal`, `ghcid`, `hlint`, `weeder`, etc.) are displayed upon entering the development shell. Type `menu` to see them again.

Adding or modifying the list of development tools can be done from [devshellProfiles.nix](./cells/prelude/devshellProfiles.nix).

#### Build and test

using cabal:

```console
$ cabal update
$ cabal build all
$ cabal test all
```

or `std` (nix):

```
$ source <(std _carapace) # (Optional, for completion on zsh).
$ std //prelude/packages/cardano-prelude:lib:cardano-prelude:build
$ std //prelude/apps/cardano-prelude-test:test:prelude-tests:run
```

(the std/nix build will use default compiler).

#### Formatting

The source tree is to be formatted with the `treefmt` command (which use [`fourmolu`](https://github.com/fourmolu/fourmolu) for haskell code).

This is checked by [Lefthook](https://github.com/evilmartians/lefthook) which will prevent commit of non-compliant files (it will also format those, so that you can retry to commit immediately).

### Commit message

Commit messages should ideally be of the form `<type>[(<scope>)]: <short description>]` ([conventionnal](https://www.conventionalcommits.org/) commit message). Available commit types are from [std presets](https://std.divnix.com/reference/presets/nixago/conform.html).

This is NOT enforced (but could be at a later point, cf. [nixago.nix](./cells/_automation/nixago.nix)) by Lefhook.

#### CI

Running CI jobs locally can be done with [tullia](https://github.com/input-output-hk/tullia):

```console
$ tullia run ci
```

If this does not work (still WIP for local execution), then use `std` to build the required nix job:

```console
$ std //_automation/ciJobs/required:build
```

#### Updating cabal index-state

If you need to update [cabal.project](./cabal.project) `index-state`, you will probably need to update the nix pin for hackage so that the new `index-state` be available to the nix build:

```console
$ nix flake lock --update-input hackage
```

You may also check what is the last `index-state` available within the current hackage pin by running:

```console
$ nix eval .#last-index-state
```

## Roles and Responsibilities

The `cardano-prelude` package is collectively mainntained by the `cardano-core`
team.  If you open a PR expect a review from one of the core contributors.


## Releases

`cardano-prelude` is released to [Cardano Haskell Packages (CHaP)][CHaP].

If you want to include it as a dependency of your project see [CHaP]
documentation.

[CHaP]: https://github.com/input-output-hk/cardano-haskell-packages
