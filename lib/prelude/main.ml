open Base
open Orcml
open Env
open Value

let to_string = function
  | VConst(Const.String x) -> x
  | VConst(Const.Int x) -> Int.to_string x
  | VConst(Const.Float x) -> Float.to_string x
  (* TODO *)
  | _ -> "<value>"


let () =
  register_ffi "core.let" (function
      | [| |] -> PrimVal(VConst Const.Signal)
      | [| v |] -> PrimVal v
      | vals -> PrimVal(VTuple (Array.to_list vals)));
  register_ffi "core.plus" (function
      | [| VConst(Const.Int x); VConst(Const.Int y) |] ->
        PrimVal (VConst(Const.Int(x + y)))
      | [| VConst(Float x); VConst(Const.Float y) |] ->
        PrimVal (VConst(Const.Float(Float.(x + y))))
      | [| VConst(Int x); VConst(Const.Float y) |] ->
        PrimVal (VConst(Const.Float(Float.(of_int x + y))))
      | [| VConst(Float x); VConst(Const.Int y) |] ->
        PrimVal (VConst(Const.Float(Float.(x + of_int y))))
      | [| VConst(Const.String x); other |] -> PrimVal (VConst(Const.String(String.concat [x; to_string other])))
      | [| VRecord(pairs1); VRecord(pairs2) |] ->
        let merged = List.fold pairs2 ~init:pairs1 ~f:(fun acc (a, b) ->
            List.Assoc.add acc ~equal:String.equal a b) in
        PrimVal(VRecord merged)
      | _ -> PrimUnsupported)
