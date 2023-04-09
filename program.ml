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

module Interpreter = struct
  type t = (register, value) Hashtbl.t

  and value =
    | Known of int
    | Unknown

  let format_value = function
    | Known i -> string_of_int i
    | Unknown -> "?"

  let format state =
    Hashtbl.fold (fun r v l ->
      sprintf "%s: %3s" (format_register r) (format_value v) :: l) state []
    |> List.sort Stdlib.compare
    |> String.concat ", "

  let value_of_operand state = function
    | Literal i -> Known i
    | Register r -> Hashtbl.find state r

  let eval op a b =
    match op, a, b with
    | "+", Known i, Known j -> Known (i + j)
    | "*", Known i, Known j -> Known (i * j)
    | "/", Known i, Known j -> Known (i / j)
    | "%", Known i, Known j -> Known (i mod j)
    | "=", Known i, Known j -> Known (Bool.to_int (i = j))
    | "*", Unknown, Known 0
    | "*", Known 0, Unknown -> Known 0
    | _, Unknown, _
    | _, _, Unknown -> Unknown
    | _ -> invalid_arg "eval"

  let initialize value =
    let state = Hashtbl.create 5 in
    Hashtbl.add state W value;
    Hashtbl.add state X value;
    Hashtbl.add state Y value;
    Hashtbl.add state Z value;
    state

  let interpret ?(verbose = false) state instr =
    let value = value_of_operand state in
    let r, v = match instr with
      | Inp r -> r, Unknown
      | Add (r, o) -> r, eval "+" (value (Register r)) (value o)
      | Mul (r, o) -> r, eval "*" (value (Register r)) (value o)
      | Div (r, o) -> r, eval "/" (value (Register r)) (value o)
      | Mod (r, o) -> r, eval "%" (value (Register r)) (value o)
      | Eql (r, o) -> r, eval "=" (value (Register r)) (value o)
    in
    (* r <- v *)
    Hashtbl.replace state r v;
    if verbose then
      printf "%s, %s\n"
        (format_instruction instr)
        (format state);
    state

  let run ?(verbose = true) =
    List.fold_left (interpret ~verbose) (initialize (Known 0))
end
