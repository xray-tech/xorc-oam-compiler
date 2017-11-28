open Base

type prim =
  | Let | Add | Sub | Ift | Iff
  | Mult | Div | Mod | Pow
  | Eq | NotEq | GT | GTE | LT | LTE
  | And | Or | Not
  | FieldAccess | MakeTuple | MakeList | MakeRecord
  | ArityCheck | ListSizeCheck | First | Rest
[@@deriving compare, enumerate, sexp]

type binding =
  | BindVar
  | BindCoeffect
  | BindFun of int
  | BindSite of string
  | BindPrim of prim [@@deriving variants, sexp]

type ctx = (string * binding) list [@@deriving sexp]

let prims_map = [("Let", Let);
                 ("Ift", Ift);
                 ("Iff", Iff);
                 ("+", Add);
                 ("-", Sub);
                 ("*", Mult);
                 ("/", Div);
                 ("%", Mod);
                 ("**", Pow);
                 ("/=", NotEq);
                 ("=", Eq);
                 ("<:", LT);
                 ("<=", LTE);
                 (":>", GT);
                 (">=", GTE);
                 ("&&", And);
                 ("||", Or);
                 ("~", Not);
                 ("'FieldAccess", FieldAccess);
                 ("'MakeTuple", MakeTuple);
                 ("'MakeList", MakeList);
                 ("'MakeRecord", MakeRecord);
                 ("'Ift", Ift);
                 ("'Iff", Iff);
                 ("'ArityCheck", ArityCheck);
                 ("'ListSizeCheck", ListSizeCheck);
                 ("'First", First);
                 ("'Rest", Rest)]

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
    | (_, BindFun(_))::xs | (_,BindPrim(_))::xs  | (_,BindSite(_))::xs | (_,BindCoeffect)::xs ->
      find acc xs in
  find 0 ctx

let ctx_find ctx ident =
  let rec find acc = function
    | [] -> None
    | (ident', bind)::xs when String.equal ident ident' ->
      Some(ctx_vars ctx - 1 - acc, bind)
    | (_, BindVar)::xs -> find (acc + 1) xs
    | _::xs -> find acc xs in
  find 0 ctx

let ctx_find_exn ctx pos ident =
  match ctx_find ctx ident with
  | None -> Errors.(throw (UnboundVar { var = ident; pos }))
  | Some(v) -> v

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

let free_vars init e =
  let rec step ctx (e, (ast_e, pos)) =
    let open! Ir1 in
    let if_out_of_scope x =
      match ctx_find ctx x with
      | Some(_) -> []
      | None -> [x] in
    match e with
    | EIdent x -> if_out_of_scope x
    | EParallel(e1, e2) | EOtherwise(e1, e2)
    | EPruning(e1, None, e2) | ESequential(e1, None, e2) ->
      step ctx e1 @ step ctx e2
    | EPruning(e1, Some(bind), e2) ->
      let ctx' = (bind, BindVar)::ctx in
      step ctx' e1 @ step ctx e2
    | ESequential(e1, Some(bind), e2) ->
      let ctx' = (bind, BindVar)::ctx in
      step ctx e1 @ step ctx' e2
    | ESite(bind, def, e) ->
      let ctx' = (bind, BindSite(def))::ctx in
      step ctx' e
    | EFix(funs, e) ->
      let ctx' = (List.map funs (fun (bind, _, _) -> (bind, BindVar))) @ ctx in
      step ctx' e @ (List.concat_map funs (fun (_, params, e) ->
          let ctx'' = List.map params (fun p -> (p, BindVar)) @ ctx' in
          step ctx'' e))
    | ECall(target, params) ->
      (if_out_of_scope target) @ (List.concat_map params if_out_of_scope)
    | EConst _ | EStop-> [] in
  step (List.map init (fun ident -> (ident, BindVar))) e

let closure_vars ctx pos vars =
  List.filter vars (fun v ->
      match ctx_find_exn ctx pos v with
      | (_, BindVar) -> true
      | _ -> false)

let rec compile_e ctx shift current_fun tail_call (e, (ast_e, pos)) =
  let is_tail_call = tail_call && true in
  let not_tail_call = tail_call && false in
  let open! Ir1 in
  match e with
  | EIdent x ->
    (match ctx_find_exn ctx pos x with
     | (i, BindVar) -> (0, [Inter.Call(prim_target Let, [| i |])])
     | (_, BindFun i) -> (0, [Inter.Closure(i, 0)])
     | (_, BindCoeffect) | (_, BindSite(_)) -> raise Util.TODO
     | (_, BindPrim prim)  ->
       (* translate to closure *)
       raise Util.TODO
    )
  | EConst v -> (0, [Inter.Const v])
  | EParallel(e1, e2) ->
    let (s2, e2') = compile_e ctx shift current_fun  is_tail_call e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') current_fun  is_tail_call e1 in
    ((Int.max s1 s2),
     Inter.Parallel(shift + len e1' + len e2' - 1,
                    shift + len e2' - 1)
     ::(e1' @ e2'))
  | EOtherwise(e1, e2) ->
    let (s2, e2') = compile_e ctx shift current_fun  is_tail_call e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') current_fun  not_tail_call e1 in
    ((Int.max s1 s2),
     Inter.Otherwise(shift + len e1' + len e2' - 1,
                     shift + len e2' - 1)
     ::(e1' @ e2'))
  | EPruning(e1, v, e2) ->
    let (ctx', index) = match v with
      | None -> (ctx, None)
      | Some(v) -> ((v, BindVar)::ctx, Some(new_index ctx)) in
    let (s2, e2') = compile_e ctx' shift current_fun  not_tail_call e2 in
    let (s1, e1') = compile_e ctx' (shift + len e2') current_fun  is_tail_call e1 in
    ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
     Inter.Pruning(shift + len e1' + len e2' - 1,
                   index,
                   shift + len e2' -1)
     ::(e1' @ e2'))
  | ESequential(e1, v, e2) ->
    let (ctx', index) = match v with
      | None -> (ctx, None)
      | Some(v) -> ((v, BindVar)::ctx, Some(new_index ctx)) in
    let (s2, e2') = compile_e ctx' shift current_fun  is_tail_call e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') current_fun  not_tail_call e1 in
    ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
     Inter.Sequential(shift + len e1' + len e2' - 1,
                      index,
                      shift + len e2' - 1)
     ::(e1' @ e2'))
  | EFix(fs, e) ->
    let vars = List.map fs (fun (ident, _, _) -> ident) in
    let closure_vars = List.map fs (fun (_, params, e) ->
        let all = free_vars (vars @ params) e in
        closure_vars ctx pos all) in
    let binds = List.map2_exn vars closure_vars (fun ident vars ->
        match vars with
        | [] -> (ident, bindfun (new_label ()))
        | vars -> (ident, BindVar)) in
    let ctx' = binds @ ctx in
    let (s, body) = compile_e ctx' shift current_fun is_tail_call e in
    let closure_size = ctx_vars ctx' in
    let make_fun (i, index, acc) (_, bind) (ident, params, body) =
      match bind with
      | BindFun(label) ->
        add_to_queue label ctx' params body;
        (i, index, acc)
      | BindVar ->
        let label = new_label () in
        add_to_queue label ctx' params body;
        (i + 2, index + 1, Inter.Pruning(i - 1, Some(index), i)::Inter.Closure(label, closure_size)::acc)
      | _ -> assert false in
    let init = (shift + len body, new_index ctx, []) in
    let (_, _, closures) = List.fold2_exn (List.rev binds) (List.rev fs) ~init ~f:make_fun in
    (s + (len closures / 2), closures @ body)
  | ECall(ident, args) ->
    let args' = List.map args (fun arg ->
        match ctx_find_exn ctx pos arg with
        | (i, BindVar) -> i
        | _ -> raise Util.TODO )in
    (match ctx_find_exn ctx pos ident with
     | (_, BindPrim p) ->
       (0, [Inter.Call(Inter.TPrim(prim_index p), Array.of_list args')])
     | (_, BindFun c) when Int.equal c current_fun ->
       (0, [Inter.TailCall(Inter.TFun(c), Array.of_list args')])
     | (_, BindFun c) ->
       (0, [Inter.Call(Inter.TFun(c), Array.of_list args')])
     | (i, BindVar) ->
       (0, [Inter.Call(Inter.TClosure(i), Array.of_list args')])
     | (i, BindSite(_)) ->
       raise Util.TODO
     | (_, BindCoeffect) ->
       (match args' with
        | [i] -> (0, [Inter.Coeffect i])
        | _ -> raise Util.TODO))
  | EStop -> (0, [Inter.Stop])
  | ESite(_, _, _) -> raise Util.TODO

let compile_fun ctx ident params body =
  let params' = List.map params (fun n -> (n, BindVar)) in
  let ctx' = params' @ ctx  in
  let (size, f) = compile_e ctx' 0 ident true body in
  (size + List.length params + ctx_vars ctx, f)

let change_labels mapping code =
  let change = function
    | Inter.Closure((label, size)) ->
      let addr = List.Assoc.find_exn mapping ~equal:Int.equal label in
      Inter.Closure((addr, size))
    | Inter.Call(Inter.TFun(label), args) ->
      let addr = List.Assoc.find_exn mapping ~equal:Int.equal label in
      Inter.Call(Inter.TFun(addr), args)
    | Inter.TailCall(Inter.TFun(label), args) ->
      let addr = List.Assoc.find_exn mapping ~equal:Int.equal label in
      Inter.TailCall(Inter.TFun(addr), args)
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
      let f = compile_fun ctx label params body in
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
