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
    | Input of number
    | Known of int
    | Unknown of number

  and number = int

  let numbers = gen_number 1

  let format_value = function
    | Input i -> "I_" ^ string_of_int i
    | Known i -> string_of_int i
    | Unknown i -> "?_" ^ string_of_int i

  let format_register_value r v =
    sprintf "%s: %3s" (format_register r) (format_value v)

  let format state =
    [W; X; Y; Z]
    |> List.map (fun r -> format_register_value r (Hashtbl.find state r))
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

    | "+", Known 0, Input i
    | "+", Input i, Known 0 -> Input i
    | "*", Known 0, Input _
    | "*", Input _, Known 0 -> Known 0
    | "*", Known 1, Input i
    | "*", Input i, Known 1 -> Input i
    | "/", Known 0, Input _ -> Known 0
    | "/", Input i, Known 1 -> Input i
    | "/", Input i, Input j when i == j -> Known 1
    | "%", Known 0, Input _ -> Known 0
    | "%", Input _, Known 1 -> Known 0
    | "%", Input i, Input j when i == j -> Known 0
    | "=", Input i, Input j when i == j -> Known 1

    | "+", Known 0, Unknown i
    | "+", Unknown i, Known 0 -> Unknown i
    | "*", Known 0, Unknown _
    | "*", Unknown _, Known 0 -> Known 0
    | "*", Known 1, Unknown i
    | "*", Unknown i, Known 1 -> Unknown i
    | "/", Known 0, Unknown _ -> Known 0
    | "/", Unknown i, Known 1 -> Unknown i
    | "/", Unknown i, Unknown j when i == j -> Known 1
    | "%", Known 0, Unknown _ -> Known 0
    | "%", Unknown _, Known 1 -> Known 0
    | "%", Unknown i, Unknown j when i == j -> Known 0
    | "=", Unknown i, Unknown j when i == j -> Known 1
    | "=", Input i, Unknown j
    | "=", Unknown i, Input j when i == j -> Known 1

    | _ -> Unknown (numbers ())

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
      | Inp r -> r, Input (numbers ())
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

let optimize_O2 program =
  let open Interpreter in
  let state = ref (initialize (Known 0)) in
  let value = value_of_operand !state in
  let redundant = function
    | Add (_, o) when value o = Known 0 -> true
    | Mul (r, _) when value (Register r) = Known 0 -> true
    | Mul (_, o) when value o = Known 1 -> true
    | Div (r, _) when value (Register r) = Known 0 -> true
    | Div (_, o) when value o = Known 1 -> true
    | Mod (r, o) -> (
      match value (Register r), value o with
      | Known a, Known b when a < b -> true
      | _ -> false
      )
    | Eql (r, o) -> (
      match value (Register r), value o with
      | Known a, Known b when a == b && a == 1 -> true
      | Known a, Known b when a <> b && a == 0 -> true
      | _ -> false
      )
    | i -> state := interpret !state i; false
  in
  optimize redundant program
