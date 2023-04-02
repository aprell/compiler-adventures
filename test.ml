open Alu
open Tests
open Utils

let test_parse_and_print () =
  let run = Parse.program >> Program.print in
  List.iter (run >> print_newline) tests

let () =
  test_parse_and_print ()
