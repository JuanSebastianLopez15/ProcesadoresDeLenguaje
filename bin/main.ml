(* bin/main.ml *)
open Lib

let compile_file input_path output_path =
  let ic = open_in input_path in
  let lexbuf = Lexing.from_channel ic in
  try
    (* 1. Parsing *)
    let prog = Parser.program Lexer.read lexbuf in

    (* 2. Validación del subset *)
    (match Subset_check.validate prog with
     | Error msg ->
         Printf.eprintf "Error de subset: %s\n" msg;
         exit 1
     | Ok _ -> ());

    (* 3. Typecheck *)
    (match Typecheck.check_program prog with
     | Error msg ->
         Printf.eprintf "Error de tipos: %s\n" msg;
         exit 1
     | Ok _ -> ());

    (* 4. Codegen *)
    let ocaml_code = Codegen.generate prog in

    (* 5. Escribir salida *)
    let oc = open_out output_path in
    output_string oc ocaml_code;
    close_out oc;
    close_in ic;
    Printf.printf "✓ Compilación exitosa: %s\n" output_path

  with
  | Lexer.LexError msg ->
      Printf.eprintf "Error léxico: %s\n" msg;
      exit 1
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Error de sintaxis en línea %d, columna %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
      exit 1

let () =
  match Sys.argv with
  | [| _; input; output |] -> compile_file input output
  | [| _; input |] ->
      let output = Filename.chop_extension input ^ ".ml" in
      compile_file input output
  | _ ->
      Printf.eprintf "Uso: %s archivo.go [salida.ml]\n" Sys.argv.(0);
      exit 1
