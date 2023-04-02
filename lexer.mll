{
  open Parser

  exception Error of string

  let position lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    pos.pos_lnum, pos.pos_cnum - pos.pos_bol

  let lexing_error lexbuf =
    let input = Lexing.lexeme lexbuf in
    let line, col = position lexbuf in
    let msg = Printf.sprintf "%d:%d: unexpected '%s'" line col input in
    raise (Error msg)
}

let whitespace = [' ' '\t']+
let newline = ['\n']
let digit = ['0'-'9']
let sign = ['+' '-']
let number = sign? digit+
let register = ['w'-'z']

rule read = parse
  | whitespace       { read lexbuf }
  | newline          { Lexing.new_line lexbuf; read lexbuf }
  | "inp"            { INP }
  | "add"            { ADD }
  | "mul"            { MUL }
  | "div"            { DIV }
  | "mod"            { MOD }
  | "eql"            { EQL }
  | number as n      { LITERAL (int_of_string n) }
  | register as r    { REGISTER r }
  | eof              { EOF }
  | _                { lexing_error lexbuf }
