open Alu
open Tests
open Utils

let () =
  let run = Parse.program >> Program.(optimize_O2 >> print) in
  List.iter (run >> print_newline) tests
