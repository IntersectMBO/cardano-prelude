{
  cell,
  inputs,
}: {
  "cardano-prelude/ci" = {
    task = "ci";
    io = ''
      #lib.io.github_push
      #input: "${cell.library.actionCiInputName}"
      #repo: "input-output-hk/cardano-prelude"
    '';
  };
}
