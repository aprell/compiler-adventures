open Alu
open Tests
open Utils

let () =
  let run = Parse.program >> Program.print in
  List.iter (run >> print_newline) tests

