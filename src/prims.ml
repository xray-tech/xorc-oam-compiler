open Base
open Inter0

let to_string = function
  | VConst(Ast.String x) -> x
  | VConst(Ast.Int x) -> Int.to_string x
  | VConst(Ast.Float x) -> Float.to_string x
    (* TODO *)
  | _ -> "<value>"

let default = [|
  (* Let *)
  (function
    | [| |] -> PrimVal(VConst Ast.Signal)
    | [| v |] -> PrimVal v
    | vals -> PrimVal(VTuple (Array.to_list vals)));
  (* Add *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Int(x + y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.(x + y))))
    | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.(of_int x + y))))
    | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Float(Float.(x + of_int y))))
    | [| VConst(Ast.String x); other |] -> PrimVal (VConst(Ast.String(String.concat [x; to_string other])))
    | [| VRecord(pairs1); VRecord(pairs2) |] ->
      let merged = List.fold pairs2 ~init:pairs1 ~f:(fun acc (a, b) ->
          List.Assoc.add acc ~equal:String.equal a b) in
      PrimVal(VRecord merged)
    | _ -> PrimUnsupported);
  (* Sub *)
  (function
    | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int(-x)))
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Int(x - y)))
    | [| VConst(Ast.Float x) |] ->
      PrimVal (VConst(Ast.Float(Float.(-x))))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.sub x y)))
    | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.(of_int x - y))))
    | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Float(Float.(x - of_int y))))
    | _ -> PrimUnsupported);
  (* Ift *)
  (function
    | [| VConst(Ast.Bool true) |] -> PrimVal (VConst(Ast.Signal))
    | [| VConst(Ast.Bool false) |] -> PrimHalt
    | _ -> PrimUnsupported);
  (* Iff *)
  (function
    | [| VConst(Ast.Bool false) |] -> PrimVal (VConst(Ast.Signal))
    | [| VConst(Ast.Bool true) |] -> PrimHalt
    | _ -> PrimUnsupported);
  (* Mult *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Int(x * y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.(x * y))))
    | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.(of_int x * y))))
    | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Float(Float.(x * of_int y))))
    | _ -> PrimUnsupported);
  (* Div *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Int(x / y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.(x / y))))
    | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.(of_int x / y))))
    | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Float(Float.(x / of_int y))))
    | _ -> PrimUnsupported);
  (* Mod *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Int(x % y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.mod_float x y)))
    | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
      PrimVal (VConst(Ast.Float(Float.mod_float (Float.of_int x) y)))
    | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Float(Float.mod_float x (Float.of_int y))))
    | _ -> PrimUnsupported);
  (* Pow *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Int(Int.pow x y)))
    (* TODO float pow *)
    | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
      PrimVal (VConst(Ast.Float(Float.int_pow x y)))

    | _ -> PrimUnsupported);
  (* Eq *)
  (function
    | [| v1; v2 |] -> PrimVal (VConst(Ast.Bool (Polymorphic_compare.equal v1 v2)))
    | _ -> PrimUnsupported);
  (* NotEq *)
  (function
    | [| VConst(v1); VConst(v2) |] ->
      PrimVal (VConst(Ast.Bool (not (Polymorphic_compare.equal v1 v2))))
    | _ -> PrimUnsupported);
  (* GT *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x > y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x > y)))
    | _ -> PrimUnsupported);
  (* GTE *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x >= y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x >= y)))
    | _ -> PrimUnsupported);
  (* LT *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x < y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x >= y)))
    | _ -> PrimUnsupported);
  (* LTE *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x <= y)))
    | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x >= y)))
    | _ -> PrimUnsupported);
  (* And *)
  (function
    | [| VConst(Ast.Bool x); VConst(Ast.Bool y) |] -> PrimVal (VConst(Ast.Bool(x && y)))
    | _ -> PrimUnsupported);
  (* Or *)
  (function
    | [| VConst(Ast.Bool x); VConst(Ast.Bool y) |] -> PrimVal (VConst(Ast.Bool(x || y)))
    | _ -> PrimUnsupported);
  (* Not *)
  (function
    | [| VConst(Ast.Bool x) |] -> PrimVal (VConst(Ast.Bool(not x)))
    | _ -> PrimUnsupported);
  (* Floor *)
  (function
    | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int x))
    | [| VConst(Ast.Float x) |] -> PrimVal (VConst(Ast.Int (Float.round_down x |> Int.of_float)))
    | _ -> PrimUnsupported);
  (* Ceil *)
  (function
    | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int x))
    | [| VConst(Ast.Float x) |] -> PrimVal (VConst(Ast.Int (Float.round_up x |> Int.of_float)))
    | _ -> PrimUnsupported);
  (* Sqrt *)
  (function
    | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Float (Float.of_int x |> Float.sqrt)))
    | [| VConst(Ast.Float x) |] -> PrimVal (VConst(Ast.Float (Float.sqrt x)))
    | _ -> PrimUnsupported);
  (* Cons *)
  (function
    | [| x; VList(xs) |] -> PrimVal (VList(x::xs))
    | _ -> PrimUnsupported);
  (* FieldAccess *)
  (function
    | [| VRecord(pairs); VConst(Ast.String field) |] ->
      (match List.Assoc.find pairs ~equal:String.equal field with
       | Some(v) -> PrimVal v
       | None -> PrimHalt)
    | _ -> PrimUnsupported);
  (* MakeTuple *)
  (fun vals ->
     PrimVal (VTuple(Array.to_list vals)));
  (* MakeList *)
  (fun vals ->
     PrimVal (VList(Array.to_list vals)));
  (* MakeRecord *)
  (fun vals ->
     let rec make acc = function
       | [] -> PrimVal(VRecord acc)
       | (VConst(Ast.String k))::v::xs -> make ((k, v)::acc) xs
       | _ -> PrimUnsupported in
     make [] (Array.to_list vals));
  (* ArityCheck *)
  (function
    | [| VTuple(vs); VConst(Ast.Int size) |] ->
      if (Int.equal (List.length vs) size)
      then PrimVal (VConst(Ast.Signal))
      else PrimHalt
    | _ -> PrimUnsupported);
  (* ListSizeCheck *)
  (function
    | [| VList(vs); VConst(Ast.Int size) |] ->
      if (Int.equal (List.length vs) size)
      then PrimVal (VConst(Ast.Signal))
      else PrimHalt
    | _ -> PrimUnsupported);
  (* First *)
  (function
    | [| VList([]) |] ->
      PrimHalt
    | [| VList(x::_) |] ->
      PrimVal (x)
    | _ -> PrimUnsupported);
  (* Rest *)
  (function
    | [| VList([]) |] ->
      PrimHalt
    | [| VList(_::xs) |] ->
      PrimVal (VList(xs))
    | _ -> PrimUnsupported);
  (* WrapSome *)
  (function
    | [| v |] -> PrimVal(VTuple [v])
    | _ -> PrimUnsupported);
  (* UnwrapSome *)
  (function
    | [| VTuple [v] |] -> PrimVal(v)
    | _ -> PrimHalt);
  (* GetNone *)
  (function
    | [| |] -> PrimVal(VTuple [])
    | _ -> PrimUnsupported);
  (* IsNone *)
  (function
    | [| VTuple [] |] -> PrimVal(VConst(Ast.Signal))
    | _ -> PrimHalt);
  (* Error *)
  (function
    | vals ->
      Stdio.eprintf "%s\n" ([%sexp_of: v array] vals |> Sexp.to_string_hum);
      PrimHalt)
|]
