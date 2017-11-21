open! Core
include Msgpck

let rec sexp_of_t = function
  | Int i -> [%sexp_of: (string * int)] ("Int", i)
  | Int32 i -> [%sexp_of: (string * int32)] ("Int32", i)
  | Int64 i -> [%sexp_of: (string * int64)] ("Int64", i)
  | Float f -> [%sexp_of: (string * float)] ("Float", f)
  | Float32 i -> [%sexp_of: (string * int32)] ("Float32", i)
  | Uint32 i -> [%sexp_of: (string * int32)] ("Uint32", i)
  | Uint64 i -> [%sexp_of: (string * int64)] ("Uint64", i)
  | String s -> [%sexp_of: (string * string)] ("String", s)
  | Bytes s -> [%sexp_of: (string * string)] ("Bytes", s)
  | Ext(t, v) -> [%sexp_of: (string * int * string)] ("Ext", t, v)
  | Nil -> (Sexp.Atom "Nil")
  | Bool v -> [%sexp_of: (string * bool)] ("Bool", v)
  | List l -> [%sexp_of: (string * t list)] ("List", l)
  | Map pairs -> [%sexp_of: (string * (t * t) list)] ("Map", pairs)
