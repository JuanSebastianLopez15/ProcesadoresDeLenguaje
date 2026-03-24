
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | IDENT of 
# 3 "frontend/parser.mly"
       (string)
# 15 "frontend/parser.ml"
  
    | EOF
  
end

include MenhirBasics

# 1 "frontend/parser.mly"
  

# 26 "frontend/parser.ml"

type ('s, 'r) _menhir_state

and _menhir_box_dummy = 
  | MenhirBox_dummy of (unit) [@@unboxed]

let _menhir_action_1 =
  fun () ->
    (
# 7 "frontend/parser.mly"
           ( () )
# 38 "frontend/parser.ml"
     : (unit))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | IDENT _ ->
        "IDENT"
    | EOF ->
        "EOF"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_0 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_dummy =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _v = _menhir_action_1 () in
          MenhirBox_dummy _v
      | _ ->
          _eRR ()
  
end

let dummy =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_dummy v = _menhir_run_0 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
