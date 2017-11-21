open Base

type prim = Let | Add | Sub | Ift | Iff | Eq
          | FieldAccess | MakeTuple | MakeList | MakeRecord
[@@deriving compare, enumerate, sexp]

type binding =
  | BindVar
  | BindCoeffect
  | BindFun of int
  | BindPrim of prim [@@deriving variants, sexp]

type ctx = (string * binding) list [@@deriving sexp]

let prims_map = [("Let", Let);
                 ("Ift", Ift);
                 ("Iff", Iff);
                 ("+", Add);
                 ("-", Sub);
                 ("=", Eq);
                 ("'FieldAccess", FieldAccess);
                 ("'MakeTuple", MakeTuple);
                 ("'MakeList", MakeList);
                 ("'MakeRecord", MakeRecord);
                 ("'Ift", Ift);
                 ("'Iff", Iff);]

let prims = List.map prims_map (fun ((s, p)) -> (s, bindprim p))

let list_index l v ~f =
  let rec index acc = function
    | [] -> None
    | x::xs when f v x = 0 -> Some(acc)
    | _::xs -> index (acc + 1) xs in
  index 0 l

let ctx_vars ctx  =
  let rec find acc = function
    | [] -> acc
    | (_, BindVar)::xs -> find (acc + 1) xs
    | (_, BindFun(_))::xs | (_,BindPrim(_))::xs | (_,BindCoeffect)::xs ->
      find acc xs in
  find 0 ctx

let ctx_find ctx pos ident =
  let rec find acc = function
    | [] -> Errors.(throw (UnboundVar { var = ident; pos }))
    | (ident', bind)::xs when String.equal ident ident' ->
      (ctx_vars ctx - 1 - acc, bind)
    | (_, BindVar)::xs -> find (acc + 1) xs
    | _::xs -> find acc xs in
  find 0 ctx

let new_index = ctx_vars

let prim_index p =
  match list_index all_of_prim p compare_prim with
  | Some i -> i
  | None -> assert false

let prim_target p = Inter.TPrim(prim_index p)

let len = List.length

let label = ref 0

let new_label () =
  label := !label + 1;
  !label

let compile_queue = ref []

let add_to_queue label ctx params body =
  compile_queue := (label, ctx, params, body)::!compile_queue

let rec compile_e ctx shift (e, (ast_e, pos)) =
  let open! Ir1 in
  match e with
  | EIdent x ->
    (match ctx_find ctx pos x with
     | (i, BindVar) -> (0, [Inter.Call(prim_target Let, [| i |])])
     | (_, BindFun i) -> (0, [Inter.Closure(i, 0)])
     | (_, BindCoeffect) -> raise Util.TODO
     | (_, BindPrim prim)  ->
       (* translate to closure *)
       raise Util.TODO
    )
  | EConst v -> (0, [Inter.Const v])
  | EParallel(e1, e2) ->
    let (s2, e2') = compile_e ctx shift e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') e1 in
    ((Int.max s1 s2),
     Inter.Parallel(shift + len e1' + len e2' - 1,
                    shift + len e2' - 1)
     ::(e1' @ e2'))
  | EOtherwise(e1, e2) ->
    let (s2, e2') = compile_e ctx shift e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') e1 in
    ((Int.max s1 s2),
     Inter.Otherwise(shift + len e1' + len e2' - 1,
                     shift + len e2' - 1)
     ::(e1' @ e2'))
  | EPruning(e1, v, e2) ->
    let (ctx', index) = match v with
      | None -> (ctx, None)
      | Some(v) -> ((v, BindVar)::ctx, Some(new_index ctx)) in
    let (s2, e2') = compile_e ctx' shift e2 in
    let (s1, e1') = compile_e ctx' (shift + len e2') e1 in
    ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
     Inter.Pruning(shift + len e1' + len e2' - 1,
                   index,
                   shift + len e2' -1)
     ::(e1' @ e2'))
  | ESequential(e1, v, e2) ->
    let (ctx', index) = match v with
      | None -> (ctx, None)
      | Some(v) -> ((v, BindVar)::ctx, Some(new_index ctx)) in
    let (s2, e2') = compile_e ctx' shift e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') e1 in
    ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
     Inter.Sequential(shift + len e1' + len e2' - 1,
                      index,
                      shift + len e2' - 1)
     ::(e1' @ e2'))
  | EFix(fs, e) ->
    let names = List.map fs (fun (ident, _, _) ->
        (ident, BindVar)) in
    let ctx' = names @ ctx in
    let (s, body) = compile_e ctx' shift e in
    let closure_size = ctx_vars ctx' in
    let make_closure (i, index, acc) (ident, params, body) =
      let label = new_label () in
      add_to_queue label ctx' params body;
      (i + 2, index + 1, Inter.Pruning(i - 1, Some(index), i)::Inter.Closure(label, closure_size)::acc) in
    let init = (shift + len body, new_index ctx, []) in
    let (_, _, closures) = List.fold (List.rev fs) ~init ~f:make_closure in
    (s + (len closures / 2), closures @ body)
  (* (\* TODO closures *\)
   * let names = List.map fs (fun (ident, _, _) ->
   *     (ident, new_label ()))in
   * let ctx' = List.map names (fun (ident, label) -> (ident, bindfun label))
   *            @ ctx  in
   * List.iter2_exn names fs (fun (_, label) (ident, params, body) ->
   *     add_to_queue label ctx' params body);
   * compile_e ctx' shift e *)
  | ECall(ident, args) ->
    let args' = List.map args (fun arg ->
        match ctx_find ctx pos arg with
        | (i, BindVar) -> i
        | _ -> raise Util.TODO )in
    (match ctx_find ctx pos ident with
     | (_, BindPrim p) ->
       (0, [Inter.Call(Inter.TPrim(prim_index p), Array.of_list args')])
     | (_, BindFun c) ->
       (0, [Inter.Call(Inter.TFun(c), Array.of_list args')])
     | (i, BindVar) ->
       (0, [Inter.Call(Inter.TClosure(i), Array.of_list args')])
     | (_, BindCoeffect) ->
       (match args' with
        | [i] -> (0, [Inter.Coeffect i])
        | _ -> raise Util.TODO))
  | EStop -> (0, [Inter.Stop])
  | ESite(_, _, _) -> raise Util.TODO

let compile_fun ctx params body =
  let params' = List.map params (fun n -> (n, BindVar)) in
  let ctx' = params' @ ctx  in
  let (size, f) = compile_e ctx' 0 body in
  (size + List.length params + ctx_vars ctx, f)

let change_labels mapping code =
  let change = function
    | Inter.Closure((label, size)) ->
      let addr = List.Assoc.find_exn mapping ~equal:Int.equal label in
      Inter.Closure((addr, size))
    | Inter.Call(Inter.TFun(label), args) ->
      let addr = List.Assoc.find_exn mapping ~equal:Int.equal label in
      Inter.Call(Inter.TFun(addr), args)
    | op -> op in
  List.map code (fun (size, body) ->
      (size, List.map body change))

let compile' e =
  compile_queue := [];
  label := 0;
  add_to_queue !label (("Coeffect", BindCoeffect)::prims) [] e;
  let rec compile_loop acc =
    match !compile_queue with
    | [] -> acc
    | (label, ctx, params, body)::xs ->
      compile_queue := xs;
      let f = compile_fun ctx params body in
      compile_loop ((label, f)::acc) in
  let repository = compile_loop [] in
  let labels_mapping = List.mapi repository (fun addr (label, _) ->
      (label, addr)) in
  let code = (List.map repository (fun (_, f) -> f)) in
  let code' = change_labels labels_mapping code in
  Array.of_list_map code' ~f:(fun (s, f) ->
      (s, Array.of_list_rev f))

let compile e =
  Errors.try_with (fun () -> compile' e)
