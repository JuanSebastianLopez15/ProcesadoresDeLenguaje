(* middle/env.ml *)
(* Mapea nombres → tipos en un scope dado.       *)
(* Usamos listas de asociación para simplicidad. *)

open Lib.Ast

type t = (string * typ) list

let empty : t = []

(* Añade un binding al scope actual *)
let extend (name : string) (typ : typ) (env : t) : t =
  (name, typ) :: env

(* Busca un nombre — devuelve option para no lanzar excepción *)
let lookup (name : string) (env : t) : typ option =
  List.assoc_opt name env

(* Scope anidado: ejecuta f con env extendido,
   luego descarta las variables locales *)
let with_scope (env : t) (f : t -> 'a) : 'a =
  f env  (* las listas son inmutables, el scope se descarta solo *)

(* Entorno inicial con funciones built-in de Go *)
let base : t = [
  ("len",     TFunc ([TSlice TAny], [TInt]));
  ("cap",     TFunc ([TSlice TAny], [TInt]));
  ("make",    TFunc ([TAny; TInt], [TAny]));
  ("append",  TFunc ([TSlice TAny; TAny], [TSlice TAny]));
  ("println", TFunc ([TString], []));
  ("panic",   TFunc ([TString], []));
]