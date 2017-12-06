open Base
type t =
  | NoInput
  | SyntaxError of { filename : string;
                     line : int;
                     col : int }
  | UnboundVar of { var : string;
                    pos : Ast.pos }
  | UnboundDependency of { ns : string;
                           f : string }
[@@deriving sexp_of]

exception Exn of t

let create t = Error.create_s (sexp_of_t t)

let err t = Error(create t)

let try_with f =
  try Ok(f ()) with
  | Exn t -> err t

let raise t = raise (Exn t)
