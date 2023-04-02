let program lines =
  lines |> Lexing.from_string |> Parser.program Lexer.read

let file name =
  name |> Utils.read_file_into_string |> program
