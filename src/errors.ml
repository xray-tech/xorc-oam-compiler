type t =
  | NoInput
  | SyntaxError of { filename : string;
                     line : int;
                     col : int }
  | UnboundVar of { var : string;
                    pos : Ast.pos }

exception Exn of t

let try_with f =
  try Ok(f ()) with
  | Exn t -> Error(t)

let throw t = raise (Exn t)
