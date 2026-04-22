(* bin/main.ml *)
open Lib

let env_flag name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | _ -> false

let trace_compiler = env_flag "COMPILER_TRACE"

let trace_log fmt =
  if trace_compiler then Printf.eprintf ("[TRACE] " ^^ fmt ^^ "\n")
  else Printf.ifprintf stderr (fmt ^^ "\n")

let read_all_lines path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        Array.of_list (List.rev acc)
    | exception exn ->
        close_in_noerr ic;
        raise exn
  in
  loop []

let print_source_context ~path ~lines ~line ~column ~lexeme =
  let safe_line = max 1 line in
  let safe_col = max 1 column in
  Printf.eprintf " --> %s:%d:%d\n" path safe_line safe_col;
  if safe_line <= Array.length lines then (
    let source_line = lines.(safe_line - 1) in
    let marker_width = max 1 (String.length lexeme) in
    let marker =
      String.make (safe_col - 1) ' '
      ^ "^"
      ^ String.make (max 0 (marker_width - 1)) '~'
    in
    Printf.eprintf "%4d | %s\n" safe_line source_line;
    Printf.eprintf "     | %s\n" marker
  ) else
    Printf.eprintf "     | <linea no disponible>\n"

let print_backtrace () =
  let bt = Printexc.get_backtrace () in
  if bt <> "" then Printf.eprintf "Traza de excepcion:\n%s\n" bt

let compile_file input_path output_path =
  let source_lines = read_all_lines input_path in
  let ic = open_in input_path in
  let lexbuf = Lexing.from_channel ic in
  let position_of_lexbuf () =
    let pos = Lexing.lexeme_start_p lexbuf in
    let column = pos.pos_cnum - pos.pos_bol + 1 in
    (pos.pos_lnum, max 1 column)
  in
  let current_lexeme () =
    let lx = Lexing.lexeme lexbuf in
    if lx = "" then "<EOF>" else lx
  in
  try
    trace_log "Inicio de compilacion: %s" input_path;

    (* 1. Parsing *)
    trace_log "Paso 1/4: analisis sintactico";
    let prog = Parser.program Lexer.read lexbuf in

    (* 2. Validación del subset *)
    trace_log "Paso 2/4: validacion de subset";
    (match Subset_check.validate prog with
     | Error msg ->
          Printf.eprintf "Error de subset: %s\n" msg;
          print_backtrace ();
          exit 1
      | Ok _ -> ());

    (* 3. Typecheck *)
    trace_log "Paso 3/4: verificacion de tipos";
    (match Typecheck.check_program prog with
     | Error msg ->
          Printf.eprintf "Error de tipos: %s\n" msg;
          print_backtrace ();
          exit 1
      | Ok _ -> ());

    (* 4. Codegen *)
    trace_log "Paso 4/4: generacion de codigo";
    let ocaml_code = Codegen.generate prog in

    (* 5. Escribir salida *)
    let oc = open_out output_path in
    output_string oc ocaml_code;
    close_out oc;
    close_in ic;
    trace_log "Compilacion finalizada sin errores";
    Printf.printf "✓ Compilación exitosa: %s\n" output_path

  with
  | Lexer.LexError err ->
      close_in_noerr ic;
      Printf.eprintf "Error lexico: %s\n" err.message;
      Printf.eprintf "Lexema no reconocido: %S\n" err.lexeme;
      print_source_context
        ~path:input_path
        ~lines:source_lines
        ~line:err.line
        ~column:err.column
        ~lexeme:err.lexeme;
      print_backtrace ();
      exit 1
  | Parser.Error ->
      close_in_noerr ic;
      let line, column = position_of_lexbuf () in
      let lexeme = current_lexeme () in
      Printf.eprintf "Error de sintaxis: simbolo inesperado %S\n" lexeme;
      print_source_context
        ~path:input_path
        ~lines:source_lines
        ~line
        ~column
        ~lexeme;
      print_backtrace ();
      exit 1
  | exn ->
      close_in_noerr ic;
      let line, column = position_of_lexbuf () in
      let lexeme = current_lexeme () in
      Printf.eprintf "Excepcion no controlada: %s\n" (Printexc.to_string exn);
      print_source_context
        ~path:input_path
        ~lines:source_lines
        ~line
        ~column
        ~lexeme;
      print_backtrace ();
      exit 1

let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [| _; input; output |] -> compile_file input output
  | [| _; input |] ->
      let output = Filename.chop_extension input ^ ".ml" in
      compile_file input output
  | _ ->
      Printf.eprintf "Uso: %s archivo.go [salida.ml]\n" Sys.argv.(0);
      exit 1
