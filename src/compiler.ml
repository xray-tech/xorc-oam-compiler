open Common

type prim =
  | Let | Add | Sub | Ift | Iff
  | Mult | Div | Mod | Pow
  | Eq | NotEq | GT | GTE | LT | LTE
  | And | Or | Not
  | Floor | Ceil | Sqrt
  | Cons
  | FieldAccess | MakeTuple | MakeList | MakeRecord
  | ArityCheck | ListSizeCheck | First | Rest
  | WrapSome | UnwrapSome | GetNone | IsNone
  | ErrorPrim
[@@deriving compare, enumerate, sexp]

type binding =
  | BindVar
  | BindCoeffect
  | BindFun of int
  | BindNs of (string * int)
  | BindSite of string
  | BindPrim of prim [@@deriving variants, sexp]

type ctx = (string * binding) list [@@deriving sexp]

type unit = { is_closure : bool;
              ident : string;
              label : int;
              ctx : ctx;
              params : string list;
              body : Ir1.e }

module Fun = struct
  type t = {
    ns : string;
    name : string;
  } [@@deriving sexp_of, compare]

  type impl = int * Inter.t list [@@deriving sexp_of, compare]
end

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
                 (":", Cons);
                 ("floor", Floor);
                 ("ceil", Ceil);
                 ("sqrt", Sqrt);
                 ("Error", ErrorPrim);
                 ("'FieldAccess", FieldAccess);
                 ("'MakeTuple", MakeTuple);
                 ("'MakeList", MakeList);
                 ("'MakeRecord", MakeRecord);
                 ("'Ift", Ift);
                 ("'Iff", Iff);
                 ("'ArityCheck", ArityCheck);
                 ("'ListSizeCheck", ListSizeCheck);
                 ("'First", First);
                 ("'Rest", Rest);
                 ("'WrapSome", WrapSome);
                 ("'UnwrapSome", UnwrapSome);
                 ("'IsNone", IsNone);
                 ("'GetNone", GetNone)]

let prims = List.map prims_map ~f:(fun ((s, p)) -> (s, bindprim p))

let list_index l v ~f =
  let rec index acc = function
    | [] -> None
    | x::_ when f v x = 0 -> Some(acc)
    | _::xs -> index (acc + 1) xs in
  index 0 l

let ctx_vars ctx  =
  let rec find acc = function
    | [] -> acc
    | (_, BindVar)::xs -> find (acc + 1) xs
    | (_, BindFun(_))::xs | (_,BindPrim(_))::xs  | (_,BindSite(_))::xs | (_,BindCoeffect)::xs | (_, BindNs(_))::xs ->
      find acc xs in
  find 0 ctx

let ctx_find ctx ident =
  let rec find acc = function
    | [] -> None
    | (ident', bind)::_ when String.equal ident ident' ->
      Some(ctx_vars ctx - 1 - acc, bind)
    | (_, BindVar)::xs -> find (acc + 1) xs
    | _::xs -> find acc xs in
  find 0 ctx

let ctx_find_exn ctx pos ident =
  match ctx_find ctx ident with
  | None -> Errors.(raise (UnboundVar { var = ident; pos }))
  | Some(v) -> v

let new_index = ctx_vars

let prim_index p =
  match list_index all_of_prim p ~f:compare_prim with
  | Some i -> i
  | None -> assert false

let prim_target p = Inter.TPrim(prim_index p)

let len = List.length

let label = ref 0

let new_label () =
  label := !label + 1;
  !label

let compile_queue = ref []

let ns_labels = ref []

let get_ns_label fun_ =
  match List.find !ns_labels ~f:(fun (_, fun_') ->
      Fun.compare fun_ fun_' |> Int.equal 0) with
  | Some(lbl, _) -> lbl
  | None ->
    let lbl = new_label () in
    ns_labels := (lbl, fun_)::!ns_labels;
    lbl

let ns_label_pos fun_ =
  match List.findi !ns_labels ~f:(fun _ (_, fun_') ->
      Fun.compare fun_ fun_' |> Int.equal 0) with
  | Some(pos, _) -> pos
  | None -> assert false

let add_to_queue unit =
  compile_queue := unit::!compile_queue

let free_vars init e =
  let rec step ctx (e, (_ast_e, _pos)) =
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
      let ctx' = (List.map funs ~f:(fun (bind, _, _) -> (bind, BindVar))) @ ctx in
      step ctx' e @ (List.concat_map funs ~f:(fun (_, params, e) ->
          let ctx'' = List.map params ~f:(fun p -> (p, BindVar)) @ ctx' in
          step ctx'' e))
    | ERefer(_ns, fs, e) ->
      let ctx' = (List.map fs ~f:(fun n -> (n, BindVar))) @ ctx in
      step ctx' e
    | ECall(target, params) ->
      (if_out_of_scope target) @ (List.concat_map params ~f:if_out_of_scope)
    | EConst _ | EStop-> [] in
  step (List.map init ~f:(fun ident -> (ident, BindVar))) e

let closure_vars ctx pos vars =
  List.filter vars ~f:(fun v ->
      match ctx_find_exn ctx pos v with
      | (_, BindVar) -> true
      | _ -> false)

let rec compile_e ctx shift current_fun tail_call (e, (ast_e, pos)) =
  let is_tail_call = tail_call && true in
  let not_tail_call = false in
  let open! Ir1 in
  let compile_call ident args =
    let args' = List.map args ~f:(fun arg ->
        match ctx_find_exn ctx pos arg with
        | (i, BindVar) -> i
        | _ -> raise Util.TODO) in
    (match ctx_find_exn ctx pos ident with
     | (_, BindPrim p) ->
       (0, [Inter.Call(Inter.TPrim(prim_index p), Array.of_list args')])
     | (_, BindFun c) when tail_call && Int.equal c current_fun ->
       (0, [Inter.TailCall(Inter.TFun(c), Array.of_list args')])
     | (_, BindFun i) | (_, BindNs(_, i)) ->
       (0, [Inter.Call(Inter.TFun(i), Array.of_list args')])
     | (i, BindVar) ->
       (0, [Inter.Call(Inter.TDynamic(i), Array.of_list args')])
     | (_, BindSite(_)) ->
       raise Util.TODO
     | (_, BindCoeffect) ->
       (match args' with
        | [i] -> (0, [Inter.Coeffect i])
        | _ -> raise Util.TODO)) in
  let preprocess_call ident args e_info =
    let deflate arg k =
      match ctx_find_exn ctx pos arg with
      | (_, BindVar) -> k arg
      | _ ->
        let f = make_fresh () in
        (EPruning(k f, Some f, (EIdent(arg), e_info)), e_info) in
    let rec f acc = function
      | arg::args ->
        deflate arg (fun i ->
            f (i::acc) args)
      | [] -> (ECall(ident, (List.rev acc)), e_info) in
    f [] args in
  match e with
  | EIdent x ->
    (* Stdio.eprintf "--LOOKUP %s: %s\n" x (sexp_of_ctx ctx |> Sexp.to_string_hum); *)
    (match ctx_find_exn ctx pos x with
     | (i, BindVar) -> (0, [Inter.Call(prim_target Let, [| i |])])
     | (_, BindFun i) -> (0, [Inter.Label(i)])
     | (_, BindNs(_, i)) -> (0, [Inter.Label(i)])
     | (_, BindCoeffect) | (_, BindSite(_)) -> raise Util.TODO
     | (_, BindPrim prim)  -> (0, [Inter.Prim(prim_index prim)]))
  | EConst v -> (0, [Inter.Const v])
  | EParallel(e1, e2) ->
    let (s2, e2') = compile_e ctx shift current_fun is_tail_call e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') current_fun is_tail_call e1 in
    ((Int.max s1 s2),
     Inter.Parallel(shift + len e1' + len e2' - 1,
                    shift + len e2' - 1)
     ::(e1' @ e2'))
  | EOtherwise(e1, e2) ->
    let (s2, e2') = compile_e ctx shift current_fun  is_tail_call e2 in
    let (s1, e1') = compile_e ctx (shift + len e2') current_fun not_tail_call e1 in
    ((Int.max s1 s2),
     Inter.Otherwise(shift + len e1' + len e2' - 1,
                     shift + len e2' - 1)
     ::(e1' @ e2'))
  | EPruning(e1, v, e2) ->
    let (ctx', index) = match v with
      | None -> (ctx, None)
      | Some(v) -> ((v, BindVar)::ctx, Some(new_index ctx)) in
    let (s2, e2') = compile_e ctx' shift current_fun not_tail_call e2 in
    let (s1, e1') = compile_e ctx' (shift + len e2') current_fun is_tail_call e1 in
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
    let vars = List.map fs ~f:(fun (ident, _, _) -> ident) in
    let closure_vars = List.map fs ~f:(fun (_, params, e) ->
        let all = free_vars (vars @ params) e in
        closure_vars ctx pos all) in
    let binds = List.map2_exn vars closure_vars ~f:(fun ident vars ->
        match vars with
        | [] -> (ident, bindfun (new_label ()))
        | _ -> (ident, BindVar)) in
    let ctx' = binds @ ctx in
    let (s, body) = compile_e ctx' shift current_fun is_tail_call e in
    let closure_size = ctx_vars ctx' in
    let make_fun (i, index, acc) (_, bind) (ident, params, body) =
      match bind with
      | BindFun(label) ->
        add_to_queue {ident; is_closure = false; label; ctx =  ctx'; params; body};
        (i, index, acc)
      | BindVar ->
        let label = new_label () in
        add_to_queue {ident; is_closure = true; label; ctx =  ctx'; params; body};
        (i + 2, index + 1, Inter.Pruning(i - 1, Some(index), i)::Inter.Closure(label, closure_size)::acc)
      | _ -> assert false in
    let init = (shift + len body, new_index ctx, []) in
    let (_, _, closures) = List.fold2_exn (List.rev binds) (List.rev fs) ~init ~f:make_fun in
    (s + (len closures / 2), closures @ body)
  | ECall(ident, args) ->
    let to_preprocess = List.find args ~f:(fun arg ->
        match ctx_find_exn ctx pos arg with
        | (_, BindPrim(_)) | (_, BindFun(_)) | (_, BindNs(_)) -> true
        | (_, BindVar) -> false
        | _ -> raise Util.TODO) in
    (match to_preprocess with
     | Some(_) ->
       preprocess_call ident args (ast_e, pos) |> compile_e ctx shift current_fun tail_call
     | None -> compile_call ident args)
  | ERefer(ns, fs, e) ->
    let ctx' = (List.map fs ~f:(fun name ->
        let lbl = get_ns_label Fun.{ns; name} in
        (name, BindNs(ns, lbl)))) @ ctx in
    compile_e ctx' shift current_fun is_tail_call e
  | EStop -> (0, [Inter.Stop])
  | ESite(_, _, _) -> raise Util.TODO

let compile_fun {is_closure; label; ctx; params; body} =
  let params' = List.rev_map params ~f:(fun n -> (n, BindVar)) in
  let ctx' = if is_closure
    then ctx
    else List.filter ctx ~f:(function | (_, BindVar) -> false
                                      | _ -> true) in
  let ctx'' = params' @ ctx' in
  let (size, f) = compile_e ctx'' 0 label true body in
  (size + List.length params + ctx_vars ctx', f)

type linker = int -> int Or_error.t

let link repo linker =
  with_return (fun r ->
      let linker' p =
        match linker p with
        | Error(err) -> r.return(Error(err))
        | Ok(v) -> v in
      let change = function
        | Inter.Closure((p, size)) ->
          Inter.Closure((linker' p, size))
        | Inter.Label(p) ->
          Inter.Label(linker' p)
        | Inter.Call(Inter.TFun(p), args) ->
          Inter.Call(Inter.TFun(linker' p), args)
        | Inter.TailCall(Inter.TFun(p), args) ->
          Inter.TailCall(Inter.TFun(linker' p), args)
        | op -> op in
      Ok(List.map repo ~f:(fun (ident, (size, body)) ->
          (ident, (size, List.map body ~f:change)))))

type imports = (int * Fun.t) list
type repo = (string * Fun.impl) list [@@deriving sexp_of, compare]

let compile' e =
  compile_queue := [];
  label := 0;
  ns_labels := [];
  add_to_queue {ident = "'main";
                is_closure = false;
                label = !label;
                ctx = (("Coeffect", BindCoeffect)::prims);
                params = [];
                body = e};
  let rec compile_loop acc =
    match !compile_queue with
    | [] -> acc
    | ({label; ident} as unit)::xs ->
      compile_queue := xs;
      let f = compile_fun unit in
      compile_loop ((label, ident, f)::acc) in
  let repo = compile_loop [] in
  let labels_mapping = List.mapi repo ~f:(fun addr (label, _, _) ->
      (label, addr)) in
  let repo' = List.map repo ~f:(fun (_, ident, (size, body)) -> (ident, (size, body))) in
  let linker lbl =
    match List.Assoc.find labels_mapping ~equal:Int.equal lbl with
    | Some(addr) -> Ok(-addr - 1)
    | None ->
      let fun_ = List.Assoc.find_exn !ns_labels ~equal:Int.equal lbl in
      Ok(ns_label_pos fun_) in
  link repo' linker

let finalize code =
  Array.of_list_map code ~f:(fun (_ident, (s, f)) ->
      (s, Array.of_list_rev f))

let make_imports () =
  List.mapi !ns_labels ~f:(fun i (_, fun_) ->
      (i, fun_))

let compile e : (imports * repo) Or_error.t =
  let res = Or_error.try_with (fun () ->
      let open Or_error.Let_syntax in
      let%map repo = compile' e in
      (make_imports (), repo)) in
  Or_error.join res

let compile_ns e =
  let open Result.Let_syntax in
  let%map (imports, repo) = compile e in
  (* Last function is a main function and it's just stub here *)
  let repo' = List.rev repo |> List.tl_exn |> List.rev in
  (imports, repo')
