{
  cell,
  inputs,
}: let
  io = ''
    #lib.io.github_push
    #input: "${cell.library.actionCiInputName}"
    #repo: "input-output-hk/cardano-prelude"
  '';
in {
  "cardano-prelude/ci/required" = {
    task = "ci/required";
    inherit io;
  };
  "cardano-prelude/ci/nonrequired" = {
    task = "ci/nonrequired";
    inherit io;
  };
}
