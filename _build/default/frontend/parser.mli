
(* The type of tokens. *)

type token = 
  | IDENT of (string)
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val dummy: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
