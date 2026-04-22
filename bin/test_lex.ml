open Lib
open Parser

let () =
  let ic = open_in "go/01_basico_funciones.go" in
  let lexbuf = Lexing.from_channel ic in
  try
    while true do
      let tok = Lexer.read lexbuf in
      let pos = lexbuf.lex_curr_p in
      Printf.printf "Token at line %d, col %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
      if tok = EOF then exit 0
    done
  with Lexer.LexError msg -> Printf.printf "LexError: %s\n" msg
