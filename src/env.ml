open Common

type prim = (Inter0.v array -> Inter0.prim_v)

type prim_res = Inter0.prim_v =
  | PrimVal of Inter0.v
  | PrimHalt
  | PrimUnsupported

type t = {
  ffc_mapping : (string, int) Hashtbl.t;
  mutable ffc : (string * prim) array;
}

type snapshot = {
  s_ffc : int array;
}

let t =
  { ffc_mapping = Hashtbl.create (module String);
    ffc = [||];}

let snapshot ffc =
  with_return (fun r ->
      let ffc' = Array.of_list_map ffc ~f:(fun k ->
          match Hashtbl.find t.ffc_mapping k with
          | Some(x) -> x
          | None -> r.return (Error (`UnknownFFC k))) in
      Ok { s_ffc = ffc' })

let get_ffc snapshot i =
  let (_name, impl) = t.ffc.(snapshot.s_ffc.(i)) in
  impl

let get_ffc_name snapshot i =
  let (name, _impl) = t.ffc.(snapshot.s_ffc.(i)) in
  name

let register_ffc name prim =
  Hashtbl.set t.ffc_mapping ~key:name ~data:(Array.length t.ffc);
  t.ffc <- Array.append t.ffc [| (name, prim) |]

let rec to_string = function
  | Inter0.VConst(Ast.String x) -> x
  | VConst(Ast.Int x) -> Int.to_string x
  | VConst(Ast.Float x) -> Float.to_string x
  | VConst(Ast.Bool x) -> Bool.to_string x
  | VConst(Ast.Signal) -> "signal"
  | VConst(Ast.Null) -> "null"
  | VClosure (fun_, _, _) -> Printf.sprintf "<closure #%d>" fun_
  | VLabel fun_ -> Printf.sprintf "<label #%d>" fun_
  | VRecord pairs ->
    let format_pair (k, v) = Printf.sprintf "%s = %s" k (to_string v) in
    let pairs' = List.map pairs ~f:format_pair in
    Printf.sprintf "{. %s .}" (String.concat ~sep:", " pairs')
  | VTuple xs ->
    Printf.sprintf "(%s)" (String.concat ~sep:", " (List.map xs ~f:to_string))
  | VList xs ->
    Printf.sprintf "[%s]" (String.concat ~sep:", " (List.map xs ~f:to_string))
  | _ -> "<value>"

let pseudo_ffc = [ "core.make-pending"; "core.pending-read";
                   "core.realize"; "core.is-realized";
                   "core.stop-pending" ]

let () =
  List.iter pseudo_ffc ~f:(fun x ->
      register_ffc x (fun _ -> PrimUnsupported))

let core = [
  ("core.noop", fun _ -> PrimVal (VConst Signal));
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
      | [| VConst(Ast.String x); other |] ->
        PrimVal (VConst(Ast.String(String.concat [x; to_string other])))
      | [| VRecord(pairs1); VRecord(pairs2) |] ->
        let merged = List.fold pairs2 ~init:pairs1 ~f:(fun acc (a, b) ->
            List.Assoc.add acc ~equal:String.equal a b) in
        PrimVal(VRecord merged)
      | _ -> PrimUnsupported);
  ("core.sub", function
      | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int(-x)))
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |]
        -> PrimVal (VConst(Ast.Int(x - y)))
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
      | [| v1; v2 |] ->
        PrimVal (VConst(Ast.Bool (Polymorphic_compare.equal v1 v2)))
      | _ -> PrimUnsupported);
  ("core.not-eq", function
      | [| VConst(v1); VConst(v2) |] ->
        PrimVal (VConst(Ast.Bool (not (Polymorphic_compare.equal v1 v2))))
      | _ -> PrimUnsupported);
  ("core.gt", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool(x > y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x > y)))
      | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.((Float.of_int x ) > y)))
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x > (Float.of_int y))))
      | _ -> PrimUnsupported);
  ("core.gte", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool(x >= y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x >= y)))
      | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.((Float.of_int x ) >= y)))
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x >= (Float.of_int y))))
      | _ -> PrimUnsupported);
  ("core.lt", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool(x < y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x < y)))
      | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.((Float.of_int x ) < y)))
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x < (Float.of_int y))))
      | _ -> PrimUnsupported);
  ("core.lte", function
      | [| VConst(Ast.Int x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool(x <= y)))
      | [| VConst(Ast.Float x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x <= y)))
      | [| VConst(Ast.Int x); VConst(Ast.Float y) |] ->
        PrimVal (VConst(Ast.Bool Float.((Float.of_int x ) <= y)))
      | [| VConst(Ast.Float x); VConst(Ast.Int y) |] ->
        PrimVal (VConst(Ast.Bool Float.(x <= (Float.of_int y))))
      | _ -> PrimUnsupported);
  ("core.and", function
      | [| VConst(Ast.Bool x); VConst(Ast.Bool y) |] ->
        PrimVal (VConst(Ast.Bool(x && y)))
      | _ -> PrimUnsupported);
  ("core.or", function
      | [| VConst(Ast.Bool x); VConst(Ast.Bool y) |] ->
        PrimVal (VConst(Ast.Bool(x || y)))
      | _ -> PrimUnsupported);
  ("core.not", function
      | [| VConst(Ast.Bool x) |] ->
        PrimVal (VConst(Ast.Bool(not x)))
      | _ -> PrimUnsupported);
  ("core.floor", function
      | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int x))
      | [| VConst(Ast.Float x) |] ->
        PrimVal (VConst(Ast.Int (Float.round_down x |> Int.of_float)))
      | _ -> PrimUnsupported);
  ("core.ceil", function
      | [| VConst(Ast.Int x) |] -> PrimVal (VConst(Ast.Int x))
      | [| VConst(Ast.Float x) |] ->
        PrimVal (VConst(Ast.Int (Float.round_up x |> Int.of_float)))
      | _ -> PrimUnsupported);
  ("core.sqrt", function
      | [| VConst(Ast.Int x) |] ->
        PrimVal (VConst(Ast.Float (Float.of_int x |> Float.sqrt)))
      | [| VConst(Ast.Float x) |] ->
        PrimVal (VConst(Ast.Float (Float.sqrt x)))
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
        Stdio.eprintf "%s\n" ([%sexp_of: Inter0.v array]
                                vals |> Sexp.to_string_hum); PrimHalt);
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
      | _ -> PrimUnsupported);
]

let () =
  List.iter core ~f:(fun (name, f) -> register_ffc name f)

let rec to_v json =
  let open Inter0 in
  match json with
  | `Null -> VConst(Null)
  | `Bool x -> VConst(Bool x)
  | `Float x -> VConst(Float x)
  | `Int x -> VConst(Int x)
  | `String x -> VConst(String x)
  | `List x -> VList (List.map x ~f:to_v)
  | `Assoc pairs -> VRecord (List.map pairs ~f:(fun (k, v) -> (k, to_v v)))

let rec to_json v =
  let open Inter0 in
  match v with
  | VConst(Null) | VConst(Signal) -> `Null
  | VConst(Bool x) -> `Bool x
  | VConst(Int x) -> `Int x
  | VConst(Float x) -> `Float x
  | VConst(String x) -> `String x
  | VList x | VTuple x -> `List (List.map x ~f:to_json)
  | VRecord pairs -> `Assoc (List.map pairs ~f:(fun (k, v) -> (k, to_json v)))
  | VClosure _ -> `String "closure"
  | VLabel _ -> `String "label"
  | VRef _ -> `String "ref"
  | VPending _ -> `String "pending"

let _ =
  register_ffc "web.json_parse" (function
      | [| VConst(String x) |] ->
        (try PrimVal (to_v (Yojson.Basic.from_string x)) with
         | _ -> PrimHalt)
      | _ -> PrimUnsupported);
  register_ffc "web.json_generate" (function
      | [| x |] ->
        let s = Yojson.Basic.to_string (to_json x) in
        PrimVal(VConst (String s))
      | _ -> PrimUnsupported)

