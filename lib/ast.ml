(*ES EL CORASON DEL COMPILADOR*)
(*ast = Abstract Syntax Tree*)
(*cada type es una pieza del arbol*)
(*este archivo me deja con lo necesario que se le pase,
    asi es mas facil de entender para el programa, en otras palabras es
    la escructura y el codigo que se le pase es la arcilla que se le acopla a este para que se acople/forme el esqueleto 
    como queremos*)
type typ = TInt | TString | TVoid

type expr =
  | IntLit of int
  | StringLit of string
  | Ident of string
  | BinOp of expr * string * expr  
  | Call of string * expr list     

type stmt =
  | Assign of string * expr           
  | ShortDecl of string * expr        
  | Incr of string                    
  | If of expr * stmt list * stmt list 
  | For of expr * stmt list           
  | Return of expr
  | ExprStmt of expr                  

type func_decl = {
  name : string;
  params : (string * typ) list;
  ret_typ : typ;
  body : stmt list;
}

type program = {
  pkg : string;
  imports : string list;
  funcs : func_decl list;
}