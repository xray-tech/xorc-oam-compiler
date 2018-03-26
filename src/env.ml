open Common

type prim = (Inter0.v array -> Inter0.prim_v)

type prim_res = Inter0.prim_v =
  | PrimVal of Inter0.v
  | PrimHalt
  | PrimUnsupported

type t = {
  ffi_mapping : (string, int) Hashtbl.t;
  mutable ffi : (string * prim) array;
}

type snapshot = {
  s_ffi : int array;
}

let t =
  { ffi_mapping = Hashtbl.create (module String);
    ffi = [||];}

let snapshot ffi =
  with_return (fun r ->
      let ffi' = Array.of_list_map ffi ~f:(fun k ->
          match Hashtbl.find t.ffi_mapping k with
          | Some(x) -> x
          | None -> r.return (Error (`UnknownFFI k))) in
      Ok { s_ffi = ffi' })

let get_ffi snapshot i =
  let (_name, impl) = t.ffi.(snapshot.s_ffi.(i)) in
  impl

let get_ffi_name snapshot i =
  let (name, _impl) = t.ffi.(snapshot.s_ffi.(i)) in
  name

let register_ffi name prim =
  Hashtbl.set t.ffi_mapping ~key:name ~data:(Array.length t.ffi);
  t.ffi <- Array.append t.ffi [| (name, prim) |]

let to_string = function
  | Inter0.VConst(Ast.String x) -> x
  | Inter0.VConst(Ast.Int x) -> Int.to_string x
  | Inter0.VConst(Ast.Float x) -> Float.to_string x
  (* TODO *)
  | _ -> "<value>"

let pseudo_ffi = [ "core.make-pending"; "core.pending-read";
                   "core.realize"; "core.is-realize";
                   "core.stop-pending" ]

let () =
  List.iter pseudo_ffi ~f:(fun x ->
    register_ffi x (fun _ -> PrimUnsupported))

let core = [
  ("core.let", function
      | [| |] -> PrimVal(VConst Signal)
      | [| v |] -> PrimVal v
      | vals -> PrimVal(VTuple (Array.to_list vals)));
  ("core.add", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Int(x + y)))
      | [| VConst(Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.(x + y))))
      | [| VConst(Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.(of_int x + y))))
      | [| VConst(Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Float(Float.(x + of_int y))))
      | [| VConst(Ast.String x); other |] -> PrimVal (VConst(Ast.String(String.concat [x; to_string other])))
      | [| VRecord(pairs1); VRecord(pairs2) |] ->
        let merged = List.fold pairs2 ~init:pairs1 ~f:(fun acc (a, b) ->
            List.Assoc.add acc ~equal:String.equal a b) in
        PrimVal(VRecord merged)
      | _ -> PrimUnsupported);
  ("core.sub", function
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
  ("core.ift", function
      | [| VConst(Ast.Bool true) |] -> PrimVal (VConst(Ast.Signal))
      | [| VConst(Ast.Bool false) |] -> PrimHalt
      | _ -> PrimUnsupported);
  ("core.iff", function
      | [| VConst(Ast.Bool false) |] -> PrimVal (VConst(Ast.Signal))
      | [| VConst(Ast.Bool true) |] -> PrimHalt
      | _ -> PrimUnsupported);
  ("core.mult", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Int(x * y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.(x * y))))
      | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.(of_int x * y))))
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Float(Float.(x * of_int y))))
      | _ -> PrimUnsupported);
  ("core.div", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Int(x / y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.(x / y))))
      | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.(of_int x / y))))
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Float(Float.(x / of_int y))))
      | _ -> PrimUnsupported);
  ("core.mod", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Int(x % y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.mod_float x y)))
      | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Float(Float.mod_float (Float.of_int x) y)))
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Float(Float.mod_float x (Float.of_int y))))
      | _ -> PrimUnsupported);
  ("core.pow", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Int(Int.pow x y)))
      (* TODO float pow *)
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Float(Float.int_pow x y)))
      | _ -> PrimUnsupported);
  ("core.eq", function
      | [| v1; v2 |] -> PrimVal (VConst(Ast.Bool (Polymorphic_compare.equal v1 v2)))
      | _ -> PrimUnsupported);
  ("core.not-eq", function
      | [| VConst(v1); VConst(v2) |] ->
        PrimVal (VConst(Ast.Bool (not (Polymorphic_compare.equal v1 v2))))
      | _ -> PrimUnsupported);
  ("core.gt", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x > y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x > y)))
      | _ -> PrimUnsupported);
  ("core.gte", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x >= y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x >= y)))
      | _ -> PrimUnsupported);
  ("core.lt", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x < y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x >= y)))
      | _ -> PrimUnsupported);
  ("core.lte", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Bool(x <= y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] -> PrimVal (VConst(Ast.Bool Float.(x >= y)))
      | _ -> PrimUnsupported);
  ("core.and", function
      | [| VConst(Ast.Bool x); VConst(Ast.Bool y) |] -> PrimVal (VConst(Ast.Bool(x && y)))
      | _ -> PrimUnsupported);
  ("core.or", function
      | [| VConst(Ast.Bool x); VConst(Ast.Bool y) |] -> PrimVal (VConst(Ast.Bool(x || y)))
      | _ -> PrimUnsupported);
  ("core.not", function
      | [| VConst(Ast.Bool x) |] -> PrimVal (VConst(Ast.Bool(not x)))
      | _ -> PrimUnsupported);
  ("core.floor", function
      | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int x))
      | [| VConst(Ast.Float x) |] -> PrimVal (VConst(Ast.Int (Float.round_down x |> Int.of_float)))
      | _ -> PrimUnsupported);
  ("core.ceil", function
      | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int x))
      | [| VConst(Ast.Float x) |] -> PrimVal (VConst(Ast.Int (Float.round_up x |> Int.of_float)))
      | _ -> PrimUnsupported);
  ("core.sqrt", function
      | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Float (Float.of_int x |> Float.sqrt)))
      | [| VConst(Ast.Float x) |] -> PrimVal (VConst(Ast.Float (Float.sqrt x)))
      | _ -> PrimUnsupported);
  ("core.cons", function
      | [| x; VList(xs) |] -> PrimVal (VList(x::xs))
      | _ -> PrimUnsupported);
  ("core.field-access", function
      | [| VRecord(pairs); VConst(Ast.String field) |] ->
        (match List.Assoc.find pairs ~equal:String.equal field with
         | Some(v) -> PrimVal v
         | None -> PrimHalt)
      | _ -> PrimUnsupported);
  ("core.make-tuple", fun vals ->
      PrimVal (VTuple(Array.to_list vals)));
  ("core.make-list", fun vals ->
      PrimVal (VList(Array.to_list vals)));
  ("core.make-record", fun vals ->
      let rec make acc = function
        | [] -> PrimVal(VRecord acc)
        | (Inter0.VConst(Ast.String k))::v::xs -> make ((k, v)::acc) xs
        | _ -> PrimUnsupported in
      make [] (Array.to_list vals));
  ("core.arity-check", function
      | [| VTuple(vs); VConst(Ast.Int size) |] ->
        if (Int.equal (List.length vs) size)
        then PrimVal (VConst(Ast.Signal))
        else PrimHalt
      | _ -> PrimUnsupported);
  ("core.list-check-size", function
      | [| VList(vs); VConst(Ast.Int size) |] ->
        if (Int.equal (List.length vs) size)
        then PrimVal (VConst(Ast.Signal))
        else PrimHalt
      | _ -> PrimUnsupported);
  ("core.first", function
      | [| VList([]) |] ->
        PrimHalt
      | [| VList(x::_) |] ->
        PrimVal (x)
      | _ -> PrimUnsupported);
  ("core.rest", function
      | [| VList([]) |] ->
        PrimHalt
      | [| VList(_::xs) |] ->
        PrimVal (VList(xs))
      | _ -> PrimUnsupported);
  ("core.wrap-some", function
      | [| v |] -> PrimVal(VTuple [v])
      | _ -> PrimUnsupported);
  ("core.unwrap-some", function
      | [| VTuple [v] |] -> PrimVal(v)
      | _ -> PrimHalt);
  ("core.get-none", function
      | [| |] -> PrimVal(VTuple [])
      | _ -> PrimUnsupported);
  ("core.is-none", function
      | [| VTuple [] |] -> PrimVal(VConst(Ast.Signal))
      | _ -> PrimHalt);
  ("core.error", function
      | vals ->
        Stdio.eprintf "%s\n" ([%sexp_of: Inter0.v array] vals |> Sexp.to_string_hum);
        PrimHalt);
  ("core.make-ref", function
      | [| v |] -> PrimVal (VRef (ref v))
      | _ -> PrimUnsupported);
  ("core.deref", function
      | [| VRef x |] -> PrimVal !x
      | _ -> PrimUnsupported);
  ("core.set", function
      | [| VRef x; v|] ->
        x := v;
        PrimVal(VConst Ast.Signal)
      | _ -> PrimUnsupported)]

let () =
  List.iter core ~f:(fun (name, f) -> register_ffi name f)
