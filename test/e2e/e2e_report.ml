open Lib.Token

(* Resultado de la etapa de lexer para un fixture e2e. *)
type lexer_outcome =
  | Lexer_ok of token list
  | Lexer_error of string

(* Resultado de la etapa de parser para un fixture e2e. *)
type parser_outcome =
  | Parser_ok
  | Parser_error of string

(* Resultado de la etapa de typecheck para un fixture e2e. *)
type typecheck_outcome =
  | Typecheck_ok
  | Typecheck_error of string

(* Resultado de la etapa de codegen para un fixture e2e. *)
type codegen_outcome =
  | Codegen_ok
  | Codegen_error of string

let has_eof tokens = List.exists (function EOF -> true | _ -> false) tokens

(* Imprime un resumen uniforme por archivo para facilitar debugging en consola. *)
let print_summary ?codegen_outcome ~file_name ~lexer_outcome ~parser_outcome
  ~typecheck_outcome () =
  let (lexer_status, token_count, eof_flag) =
    match lexer_outcome with
    | Lexer_ok tokens -> ("ok", List.length tokens, has_eof tokens)
    | Lexer_error _ -> ("error", 0, false)
  in
  let parser_status =
    match parser_outcome with
    | Parser_ok -> "ok"
    | Parser_error _ -> "error"
  in
  let typecheck_status =
    match typecheck_outcome with
    | Typecheck_ok -> "ok"
    | Typecheck_error _ -> "error"
  in
  let lexer_detail =
    match lexer_outcome with
    | Lexer_ok _ -> ""
    | Lexer_error msg -> Printf.sprintf " | detail=%s" msg
  in
  let typecheck_detail =
    match typecheck_outcome with
    | Typecheck_ok -> ""
    | Typecheck_error msg -> Printf.sprintf " | detail=%s" msg
  in
  let codegen_status, codegen_detail =
    match codegen_outcome with
    | None -> (None, "")
    | Some Codegen_ok -> (Some "ok", "")
    | Some (Codegen_error msg) -> (Some "error", Printf.sprintf " | detail=%s" msg)
  in
  let parser_detail =
    match parser_outcome with
    | Parser_ok -> ""
    | Parser_error msg -> Printf.sprintf " | detail=%s" msg
  in
  match codegen_status with
  | None ->
      Printf.printf
        "\n[E2E] file=%s\n  lexer=%s | tokens=%d | eof=%b%s\n  parser=%s%s\n  typecheck=%s%s\n%!"
        file_name lexer_status token_count eof_flag lexer_detail parser_status
        parser_detail typecheck_status typecheck_detail
  | Some status ->
      Printf.printf
        "\n[E2E] file=%s\n  lexer=%s | tokens=%d | eof=%b%s\n  parser=%s%s\n  typecheck=%s%s\n  codegen=%s%s\n%!"
        file_name lexer_status token_count eof_flag lexer_detail parser_status
        parser_detail typecheck_status typecheck_detail status codegen_detail
