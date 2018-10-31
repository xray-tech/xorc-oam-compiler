open Base
open Orcml
open Env
open Value

let rec to_v = function
  | `Null -> VConst(Const.Null)
  | `Bool x -> VConst(Const.Bool x)
  | `Float x -> VConst(Const.Float x)
  | `Int x -> VConst(Const.Int x)
  | `String x -> VConst(Const.String x)
  | `List x -> VList (List.map x ~f:to_v)
  | `Assoc pairs -> VRecord (List.map pairs ~f:(fun (k, v) -> (k, to_v v)))

let rec to_json = function
  | VConst(Const.Null) | VConst(Const.Signal) -> `Null
  | VConst(Const.Bool x) -> `Bool x
  | VConst(Const.Int x) -> `Int x
  | VConst(Const.Float x) -> `Float x
  | VConst(Const.String x) -> `String x
  | VList x | VTuple x -> `List (List.map x ~f:to_json)
  | VRecord pairs -> `Assoc (List.map pairs ~f:(fun (k, v) -> (k, to_json v)))
  | VClosure _ | VLabel _ -> `String "<fun>"

let () =
  register_ffc "json.parse" (function
      | [| VConst(Const.String x) |] ->
        (try PrimVal (to_v (Yojson.Basic.from_string x)) with
         | _ -> PrimHalt)
      | _ -> PrimUnsupported);
  register_ffc "json.generate" (function
      | [| x |] ->
        let s = Yojson.Basic.to_string (to_json x) in
        PrimVal(VConst (Const.String s))
      | _ -> PrimUnsupported)
