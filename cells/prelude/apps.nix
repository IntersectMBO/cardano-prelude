{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) lib;
in
  lib.filterAttrs (_: p: p.meta ? mainProgram) cell.packages
