%{
%}
%token <string> IDENT
%token EOF
%start <unit> dummy
%%
dummy: EOF { () }

(*Esto es temporal, es para unas pruebas del viejo lopez, no se asuste socio*)