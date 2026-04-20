module Lexer = Frontend.Lexer
module Parser = Frontend.Parser
module Typecheck = Go_to_ocaml_middle.Typecheck
module Codegen = Codegen

let read_file path =
  let ch = open_in_bin path in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  data

let write_file path content =
  let ch = open_out path in
  output_string ch content;
  close_out ch

let () =
  (* Obtener argumentos *)
  let args = Array.to_list Sys.argv in
  let input_file = match List.nth_opt args 1 with
    | Some f -> f
    | None -> "Source Code/sufrimiento_go.txt"
  in
  let output_file = match List.nth_opt args 2 with
    | Some f -> f
    | None -> "output.ml"
  in
  
  Printf.printf "=== Traduciendo %s ===\n" input_file;
  Printf.printf "Salida: %s\n" output_file;
  
  (* Verificar que el archivo de entrada existe *)
  if not (Sys.file_exists input_file) then (
    Printf.eprintf "Error: No se encuentra el archivo de entrada: %s\n" input_file;
    Printf.eprintf "Archivos en el directorio actual:\n";
    let files = Sys.readdir "." in
    Array.iter (fun f -> Printf.eprintf "  %s\n" f) files;
    exit 1
  );
  
  let source = read_file input_file in
  
  (* Lexer y parser *)
  let lexbuf = Lexing.from_string source in
  let ast =
    try Parser.program Lexer.read lexbuf
    with
    | Lexer.SyntaxError msg ->
        let pos = lexbuf.lex_curr_p in
        Printf.eprintf "Error léxico en línea %d, columna %d: %s\n" 
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg;
        exit 1
    | Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        let token = Lexing.lexeme lexbuf in
        Printf.eprintf "Error sintáctico en línea %d, columna %d: token inesperado '%s'\n" 
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) token;
        exit 1
    | Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1
  in
  
  (* Typecheck *)
  match Typecheck.check_program ast with
  | Error msg ->
      Printf.eprintf "Error de tipos: %s\n" msg;
      exit 1
  | Ok _ -> Printf.printf "Typecheck OK\n";
  
  (* Generación de código OCaml *)
  let ocaml_code = Codegen.generate ast in
  write_file output_file ocaml_code;
  Printf.printf "¡Código OCaml generado exitosamente en %s!\n" output_file
