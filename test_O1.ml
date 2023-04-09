open Alu
open Tests
open Utils

let () =
  let run = Parse.program >> Program.(optimize_O1 >> print) in
  List.iter (run >> print_newline) tests
