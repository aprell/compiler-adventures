open Utils

type instruction =
  | Inp of register
  | Add of register * operand
  | Mul of register * operand
  | Div of register * operand
  | Mod of register * operand
  | Eql of register * operand

and operand =
  | Literal of int
  | Register of register

and register = W | X | Y | Z

let format_operator = function
  | Inp _ -> "input()"
  | Add _ -> " + "
  | Mul _ -> " * "
  | Div _ -> " / "
  | Mod _ -> " % "
  | Eql _ -> " = "

let format_register = function
  | W -> "w"
  | X -> "x"
  | Y -> "y"
  | Z -> "z"

let format_operand = function
  | Literal n -> string_of_int n
  | Register r -> format_register r

let format_instruction i =
  match i with
  | Inp r ->
    format_register r ^ " := " ^
    format_operator i
  | Add (r, o)
  | Mul (r, o)
  | Div (r, o)
  | Mod (r, o)
  | Eql (r, o) ->
    format_register r ^ " := " ^
    format_register r ^ format_operator i ^ format_operand o

let print = List.iter (format_instruction >> print_endline)

let optimize redundant = List.filter (not << redundant)

let optimize_O1 program =
  let redundant = function
    | Add (_, Literal 0)
    | Mul (_, Literal 1)
    | Div (_, Literal 1) -> true
    | _ -> false
  in
  optimize redundant program
