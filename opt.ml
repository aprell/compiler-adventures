open Alu
open Utils

let () =
  let prog = Sys.argv.(0) in
  let usage = sprintf "Usage: %s [OPTION] filename" prog in
  let options = [
    "-O0",
    Arg.String (Parse.file >> Program.print),
    "Optimize at level 0 (no optimizations)";

    "-O1",
    Arg.String (Parse.file >> Program.(optimize_O1 >> print)),
    "Optimize at level 1";

    "-O2",
    Arg.String (Parse.file >> Program.(optimize_O2 >> print)),
    "Optimize at level 2";

    "-I",
    Arg.String (Parse.file >> Program.interpret),
    "Run abstract interpreter";

    "-Iv",
    Arg.String (Parse.file >> Program.interpret ~verbose:true),
    "Run abstract interpreter (verbose output)";
  ]
  in
  (* Default is -O0 *)
  Arg.parse options (Parse.file >> Program.print) usage
