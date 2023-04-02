open Alu
open Utils

let input = ref None

let parse_args () =
  let prog = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s filename" prog in
  Arg.parse [] (fun filename -> input := Some filename) usage

let () =
  parse_args ();
  match !input with
  | Some filename ->
    let action = Parse.file >> Program.print in
    action filename
  | None -> ()
