open Alu
open Utils

let () =
  let prog = Sys.argv.(0) in
  let usage = sprintf "Usage: %s [OPTION] filename" prog in
  let options = [
    "-O0",
    Arg.String (Parse.file >> Program.compile),
    "Optimize at level 0 (no optimizations)";

    "-O1",
    Arg.String (Parse.file >> Program.(optimize_O1 >> compile)),
    "Optimize at level 1";

    "-O2",
    Arg.String (Parse.file >> Program.(optimize_O2 >> compile)),
    "Optimize at level 2";
  ]
  in
  (* Default is -O0 *)
  Arg.parse options (Parse.file >> Program.compile) usage
