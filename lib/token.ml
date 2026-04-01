(*Esto define las palabras clave/reservadas y simbolos del codigo que nos pasan de go y asi sea posible pasar a OCaml*)
(*este archivo no traduce nada aun, como que identifica las palabras y eso*)
type token =
  | PACKAGE | IMPORT | FUNC | FOR | IF | ELSE | RETURN | VAR | RANGE
  | TRUE | FALSE | NIL
  | IDENT of string
  | INTLIT of int
  | STRINGLIT of string
  | ASSIGN        (* = *)  (*son los caracteres para una mejor interpretacion, si quieren luego los quitamos*)
  | DECL_ASSIGN   (* := *)
  | PLUS | MINUS | STAR | SLASH | MOD  (* + - * / % *)
  | EQ_EQ | NOT_EQ | LT | GT | LTE | GTE   (* == != < > <= >= *)
  | AND_AND | OR_OR  (* && || *)
  | BANG          (* ! *)
  | INC | DEC     (* ++ -- *)
  | LPAREN | RPAREN (* ( ) *)
  | LBRACK | RBRACK (* [ ] *)
  | LBRACE | RBRACE (* { } *)
  | COMMA           (* , *)
  | DOT             (* . *)
  | SEMICOLON       (* ; *)
  | EOF