(*Esto define las palabras clave/reservadas y simbolos del codigo que nos pasan de go y asi sea posible pasar a OCaml*)
(*este archivo no traduce nada aun, como que identifica las palabras y eso*)
type token =
  | PACKAGE | IMPORT | FUNC | FOR | IF | ELSE | RETURN
  | IDENT of string
  | INTLIT of int
  | STRINGLIT of string
  | ASSIGN        (* = *)  (*son los caracteres para una mejor interpretacion, si quieren luego los quitamos*)
  | DECL_ASSIGN   (* := *)
  | PLUS | STAR | MOD  (* + * % *)
  | EQ_EQ | LTE   (* == <= *)
  | INC           (* ++ *)
  | LPAREN | RPAREN (* ( ) *)
  | LBRACE | RBRACE (* { } *)
  | COMMA           (* , *)
  | DOT             (* . *)
  | SEMICOLON       (* ; *)
  | EOF