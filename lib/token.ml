type token =
  | PACKAGE | IMPORT | FUNC | FOR | IF | ELSE | RETURN | VAR | RANGE
  | TYPE | STRUCT
  | TRUE | FALSE | NIL
  | IDENT of string
  | STRUCT_ID of string   (* NUEVO: Token para nombres con Mayúscula *)
  | INTLIT of int
  | STRINGLIT of string
  | ASSIGN
  | DECL_ASSIGN
  | PLUS | MINUS | STAR | SLASH | MOD
  | EQ_EQ | NOT_EQ | LT | GT | LTE | GTE
  | AND_AND | OR_OR
  | BANG
  | INC | DEC
  | LPAREN | RPAREN
  | LBRACK | RBRACK
  | LBRACE | RBRACE
  | COMMA
  | DOT
  | SEMICOLON
  | EOF