open Alu
open Utils

let input = ref None

let parse_args () =
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
  ]
  in
  Arg.parse options (fun filename -> input := Some filename) usage

let () =
  parse_args ();
  match !input with
  | Some filename ->
    (* Default is -O0 *)
    let action = Parse.file >> Program.print in
    action filename
  | None -> ()
