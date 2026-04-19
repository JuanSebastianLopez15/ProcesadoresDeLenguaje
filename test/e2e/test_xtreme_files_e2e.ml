open OUnit2
open Lib.Token

module Lexer = Frontend.Lexer
module Parser = Frontend.Parser
module Typecheck = Go_to_ocaml_middle.Typecheck
module Codegen = Codegen
module Report = E2e_report

(* E2E estricto para xtreme_tests: lexer -> parser -> typecheck -> codegen por archivo completo. *)

let xtreme_dir_candidates =
  [
    "Source Code/xtreme_tests";
    "../Source Code/xtreme_tests";
    "../../Source Code/xtreme_tests";
    "../../../Source Code/xtreme_tests";
    "../../../../Source Code/xtreme_tests";
  ]

let valid_extensions = [ ".go"; ".txt" ]

let resolve_xtreme_dir () =
  match List.find_opt Sys.file_exists xtreme_dir_candidates with
  | Some path -> path
  | None -> failwith "No se encontro carpeta Source Code/xtreme_tests"

let read_file path =
  let ch = open_in_bin path in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  data

let tokenize source =
  let lexbuf = Lexing.from_string source in
  let rec loop acc =
    try
      match Lexer.read lexbuf with
      | EOF -> Ok (List.rev (EOF :: acc))
      | t -> loop (t :: acc)
    with
    | Lexer.SyntaxError msg -> Error msg
  in
  loop []

let parse_program source =
  let lexbuf = Lexing.from_string source in
  try Ok (Parser.program Lexer.read lexbuf)
  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      Error
        (Printf.sprintf "error sintactico en linea %d, columna %d" pos.pos_lnum col)
  | Failure msg -> Error msg

let has_valid_extension file_name =
  List.exists (fun ext -> Filename.check_suffix file_name ext) valid_extensions

let list_fixture_files base_dir =
  Sys.readdir base_dir
  |> Array.to_list
  |> List.filter has_valid_extension
  |> List.sort String.compare

let run_fixture base_dir file_name _ =
  let path = Filename.concat base_dir file_name in
  let source = read_file path in

  let lexer_outcome =
    match tokenize source with
    | Ok tokens -> Report.Lexer_ok tokens
    | Error msg -> Report.Lexer_error msg
  in

  let parser_result = parse_program source in
  let parser_outcome =
    match parser_result with
    | Ok _ -> Report.Parser_ok
    | Error msg -> Report.Parser_error msg
  in

  let typecheck_outcome, ast_opt =
    match parser_result with
    | Error msg -> (Report.Typecheck_error ("parser: " ^ msg), None)
    | Ok ast ->
        (match Typecheck.check_program ast with
        | Ok _ -> (Report.Typecheck_ok, Some ast)
        | Error msg -> (Report.Typecheck_error msg, Some ast))
  in

  let codegen_outcome =
    match ast_opt, typecheck_outcome with
    | Some ast, Report.Typecheck_ok ->
        (try
           let generated = Codegen.generate ast in
           if String.trim generated = "" then
             Report.Codegen_error "codegen produjo salida vacia"
           else Report.Codegen_ok
         with
        | Failure msg -> Report.Codegen_error msg
        | exn -> Report.Codegen_error (Printexc.to_string exn))
    | None, Report.Typecheck_ok ->
        Report.Codegen_error "parser: no se pudo construir AST"
    | _, Report.Typecheck_error msg -> Report.Codegen_error ("typecheck: " ^ msg)
  in

  Report.print_summary ~file_name ~lexer_outcome ~parser_outcome
    ~typecheck_outcome ~codegen_outcome ();

  let tokens =
    match lexer_outcome with
    | Report.Lexer_ok ts -> ts
    | Report.Lexer_error msg ->
        assert_failure
          (Printf.sprintf "Lexer fallo en %s: %s" file_name msg)
  in

  assert_bool
    (Printf.sprintf "No se encontro EOF en %s" file_name)
    (List.exists (function EOF -> true | _ -> false) tokens);

  (match parser_result with
  | Ok _ -> ()
  | Error msg ->
      assert_failure
        (Printf.sprintf "Parser fallo en %s: %s" file_name msg));

  (match typecheck_outcome with
  | Report.Typecheck_ok -> ()
  | Report.Typecheck_error msg ->
      assert_failure
        (Printf.sprintf "Typecheck fallo en %s: %s" file_name msg));

  match codegen_outcome with
  | Report.Codegen_ok -> ()
  | Report.Codegen_error msg ->
      assert_failure
        (Printf.sprintf "Codegen fallo en %s: %s" file_name msg)

let suite =
  let base_dir = resolve_xtreme_dir () in
  let files = list_fixture_files base_dir in
  if files = [] then
    failwith "No se encontraron archivos xtreme (.go/.txt) en Source Code/xtreme_tests";
  "e2e_xtreme_full_pipeline"
  >::: List.map (fun file_name -> file_name >:: run_fixture base_dir file_name) files

let () = run_test_tt_main suite
