open Common

module I = Inter0
open Repository

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
    | (_, BindFun _)::xs | (_,BindCoeffect)::xs | (_, BindMod(_))::xs ->
      find acc xs in
  find 0 ctx

let ctx_find' ctx ident =
  let rec find acc = function
    | [] -> None
    | (ident', bind)::_ when String.equal ident ident' ->
      Some(ctx_vars ctx - 1 - acc, bind)
    | (_, BindVar)::xs -> find (acc + 1) xs
    | _::xs -> find acc xs in
  find 0 ctx

let ctx_find ctx pos ident =
  match ctx_find' ctx ident with
  | None -> Error (`UnboundVar (pos.Ast.pstart, ident))
  | Some(v) -> Ok v

let new_index = ctx_vars

let len = List.length

let compile_unit_equal a b =
  match (a, b) with
  | ({orc_module; ident}, {orc_module = mod'; ident = ident'})
    when String.equal mod' orc_module && String.equal ident' ident ->
    true
  | _ -> false

type state = {
  repository: Repository.t;
  mutable ffi_in_use : string list;
  mutable repo : (string * string * int * I.t array) list;
  mutable compile_queue : unit list;
}

let repo_index state mod_ ident =
  let rec f = function
    | [] -> None
    | (mod_', ident', _, _)::xs when String.equal mod_' mod_ && String.equal ident' ident ->
      Some(len xs)
    | _::xs -> f xs in
  f state.repo

let get_label state unit =
  let rec f i = function
    | [] ->
      state.compile_queue <- state.compile_queue @ [unit];
      len state.repo + i
    | {orc_module; ident}::_ when String.equal orc_module unit.orc_module && String.equal ident unit.ident ->
      len state.repo + i
    | _::xs -> f (i + 1) xs in
  match repo_index state "" unit.ident with
  | Some(i) -> i
  | None -> f 0 state.compile_queue

let get_import_label state (mod_, ident) =
  with_return (fun r ->
      let rec f i = function
        | [] ->
          (match Repository.get state.repository mod_ ident with
           | None -> r.return None
           | Some(unit) ->
             state.compile_queue <- state.compile_queue @ [unit];
             len state.repo + i)
        | {orc_module = mod_'; ident = ident'}::_ when String.equal mod_' mod_ && String.equal ident' ident ->
          len state.repo + i
        | _::xs -> f (i+1) xs in
      match repo_index state mod_ ident with
      | Some(i) -> Some(i)
      | None -> Some(f 0 state.compile_queue))

let get_ffi state def =
  let rec f = function
    | [] ->
      state.ffi_in_use <- def::state.ffi_in_use;
      len state.ffi_in_use - 1
    | x::xs when String.equal x def ->
      len xs
    | _::xs -> f xs in
  f state.ffi_in_use

let free_vars init e =
  let rec step ctx (e, (_ast_e, pos)) =
    let open! Ir1 in
    let if_out_of_scope x =
      match ctx_find' ctx x with
      | Some(_) -> []
      | None -> [(pos, x)] in
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
    | EFFI(_, params) ->
      List.concat_map params ~f:if_out_of_scope
    | EConst _ | EStop | EModule -> [] in
  step (List.map init ~f:(fun ident -> (ident, BindVar))) e

let closure_vars ctx vars =
  with_return (fun r ->
      let filtered = List.filter_map vars ~f:(fun (pos, v) ->
          match ctx_find ctx pos v with
          | Error(_) as err -> r.return err
          | Ok((_, BindVar)) -> Some(v)
          | _ -> None) in
      Ok filtered)

type env = {
  orc_module: string;
  current_fun : string;
  ctx : ctx;
  shift : int;
  tail_call : bool;
  state : state;
}

exception Module of ctx

(* Here we determine which functions are closures
   Function is closure if:
   - it uses variables from outer scope
   - it uses functions from the same scope which are closures (and this is recursive rule)*)
let analyze_functions_graph g =
  let rec f checked ident = function
    | ([], []) -> false
    | ([], edges) ->
      let unchecked = List.filter edges ~f:(fun edge ->
          not (List.mem checked edge ~equal:String.equal)) in
      List.find unchecked ~f:(fun edge ->
          let x = List.Assoc.find_exn g ~equal:String.equal edge in
          f (ident::checked) edge x) |> Option.is_some
    | _ -> true
  in
  List.map g ~f:(fun (ident, x) ->
      (ident, f [ident] ident x))

let compile_e env e =
  with_return (fun r ->
      let rec compile_e' env (e, (ast_e, pos)) =
        let ctx_find x = match ctx_find env.ctx pos x with
          | Ok(v) -> v
          | Error(_) as err -> r.return err in
        let get_import_label' (ns, ident) =
          match get_import_label env.state (ns, ident) with
          | None -> r.return (Error (`UnknownReferedFunction(ns, ident)))
          | Some(i) -> i in
        let open! Ir1 in
        let compile_call ident args =
          let args' = List.map args ~f:(fun arg ->
              match ctx_find arg with
              | (i, BindVar) -> i
              | _ -> assert false) in
          (match ctx_find ident with
           | (_, BindFun unit) when env.tail_call &&
                                    String.equal env.current_fun unit.ident &&
                                    String.equal env.orc_module unit.orc_module ->
             let c = get_label env.state unit in
             (0, [(I.TailCall(I.TFun(c), Array.of_list args'), pos)])
           | (_, BindFun unit) ->
             let c = get_label env.state unit in
             (0, [(I.Call(I.TFun(c), Array.of_list args'), pos)])
           |  (_, BindMod f) ->
             let c = get_import_label' f in
             (0, [(I.Call(I.TFun(c), Array.of_list args'), pos)])
           (* TODO tail call of closures *)
           | (i, BindVar) ->
             (0, [(I.Call(I.TDynamic(i), Array.of_list args'), pos)])
           | (_, BindCoeffect) ->
             (match args' with
              | [i] -> (0, [(I.Coeffect i, pos)])
              | _ -> raise Util.TODO)) in
        let need_preprocess args =
          List.find args ~f:(fun arg ->
              match ctx_find arg with
              | (_, BindFun(_))
              | (_, BindMod(_) | (_, BindCoeffect)) -> true
              | (_, BindVar) -> false)
          |> Option.is_some in
        let preprocess_args args e_info k =
          let deflate arg k =
            match ctx_find arg with
            | (_, BindVar) -> k arg
            | _ ->
              let f = make_fresh () in
              (EPruning(k f, Some f, (EIdent(arg), e_info)), e_info) in
          let rec f acc = function
            | arg::args ->
              deflate arg (fun i ->
                  f (i::acc) args)
            | [] -> k (List.rev acc) in
          f [] args in
        match e with
        | EModule -> raise (Module env.ctx)
        | EIdent x ->
          (match ctx_find x with
           | (_, BindVar) ->
             (EFFI ("core.let", [x]), (ast_e, pos))
             |> compile_e' env
           | (_, BindFun unit) ->
             let c = get_label env.state unit in
             (0, [(I.Label(c), pos)])
           | (_, BindMod f) ->
             let c = get_import_label' f in
             (0, [(I.Label(c), pos)])
           | (_, BindCoeffect) -> raise Util.TODO)
        | EConst v -> (0, [(I.Const v, pos)])
        | EParallel(e1, e2) ->
          let (s2, e2') = compile_e' env e2 in
          let (s1, e1') = compile_e' {env with shift = env.shift + len e2'} e1 in
          ((Int.max s1 s2),
           (I.Parallel(env.shift + len e1' + len e2' - 1,
                       env.shift + len e2' - 1), pos)
           ::(e1' @ e2'))
        | EOtherwise(e1, e2) ->
          let (s2, e2') = compile_e' env e2 in
          let (s1, e1') = compile_e' {env with shift = env.shift + len e2'} e1 in
          ((Int.max s1 s2),
           (I.Otherwise(env.shift + len e1' + len e2' - 1,
                        env.shift + len e2' - 1), pos)
           ::(e1' @ e2'))
        | EPruning(e1, v, e2) ->
          let (ctx', index) = match v with
            | None -> (env.ctx, None)
            | Some(v) -> ((v, BindVar)::env.ctx, Some(new_index env.ctx)) in
          (* FIXME ctx' in right branch? wrong *)
          let (s2, e2') = compile_e' { env with tail_call = false; ctx = ctx' } e2 in
          let (s1, e1') = compile_e' { env with shift = env.shift + len e2'; ctx = ctx'} e1 in
          ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
           (I.Pruning(env.shift + len e1' + len e2' - 1,
                      index,
                      env.shift + len e2' -1), pos)
           ::(e1' @ e2'))
        | ESequential(e1, v, e2) ->
          let (ctx', index) = match v with
            | None -> (env.ctx, None)
            | Some(v) -> ((v, BindVar)::env.ctx, Some(new_index env.ctx)) in
          let (s2, e2') = compile_e' { env with ctx = ctx' } e2 in
          let (s1, e1') = compile_e' { env with tail_call = false; shift = env.shift + len e2'} e1 in
          ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
           (I.Sequential(env.shift + len e1' + len e2' - 1,
                         index,
                         env.shift + len e2' - 1), pos)
           ::(e1' @ e2'))
        | EFix(fs, e) ->
          let vars = List.map fs ~f:(fun (ident, _, (_, (_, pos))) -> ident) in
          let closure_vars = List.map fs ~f:(fun (ident, params, e) ->
              let all = free_vars (vars @ params) e in
              let all_plus_fix = free_vars params e |> List.map ~f:(fun (_, x) -> x) in
              let all_only_names = all |> List.map ~f:(fun (_, x) -> x) in
              let fix_links = List.filter all_plus_fix ~f:(fun x ->
                  List.mem all_only_names x ~equal:String.equal |> not) in
              match closure_vars env.ctx all with
              | Ok v -> (ident, (v, fix_links))
              | Error _ as err -> r.return err) in
          let closures = analyze_functions_graph closure_vars in
          let binds = List.map2_exn fs closures ~f:(fun (ident, params, body) (_, is_closure) ->
              if is_closure
              then (ident, BindVar)
              else (ident, BindFun {orc_module = env.orc_module;
                                    ident;
                                    is_closure;
                                    ctx = ref None;
                                    params; body})) in
          let ctx' = binds @ env.ctx in
          List.iter binds ~f:(function | (_, BindFun { ctx }) -> ctx := Some ctx' | _ -> ());
          let (s, body) = compile_e' { env with ctx = ctx'} e in
          let closure_size = ctx_vars ctx' in
          let make_fun (i, index, acc) (_, bind) (ident, params, body) =
            match bind with
            | BindFun _ ->
              (i, index, acc)
            | BindVar ->
              let c = get_label env.state {orc_module = env.orc_module;
                                           ident;
                                           is_closure = true;
                                           ctx = ref (Some ctx');
                                           params; body} in
              (i + 2, index + 1, (I.Pruning(i - 1, Some(index), i), pos)::(I.Closure(c, closure_size), pos)::acc)
            | _ -> assert false in
          let init = (env.shift + len body, new_index env.ctx, []) in
          let (_, _, closures) = List.fold2_exn (List.rev binds) (List.rev fs) ~init ~f:make_fun in
          (s + (len closures / 2), closures @ body)
        | ECall(ident, args) ->
          if need_preprocess args
          then preprocess_args args (ast_e, pos) (fun args' ->
              (ECall(ident, args'), (ast_e, pos)))
               |> compile_e' env
          else compile_call ident args
        | ERefer(ns, fs, e) ->
          let ctx' = (List.map fs ~f:(fun name ->
              (name, BindMod(ns, name)))) @ env.ctx in
          compile_e' { env with ctx = ctx' } e
        | EFFI(def, args) ->
          if need_preprocess args
          then preprocess_args args (ast_e, pos) (fun args' ->
              (EFFI(def, args'), (ast_e, pos)))
               |> compile_e' env
          else
            let args' = List.map args ~f:(fun arg ->
                match ctx_find arg with
                | (i, BindVar) -> i
                | _ -> assert false) in
            (0, [(I.FFI(get_ffi env.state def, Array.of_list args'), pos)])
        | EStop -> (0, [(I.Stop, pos)]) in
      Ok(compile_e' env e))

let compile_fun state {orc_module; ident; is_closure; ctx; params; body} =
  let params' = List.rev_map params ~f:(fun n -> (n, BindVar)) in
  let ctx = Option.value_exn !ctx in
  let ctx' = if is_closure
    then ctx
    else List.filter ctx ~f:(function | (_, BindVar) -> false
                                      | _ -> true) in
  let ctx'' = params' @ ctx' in
  let open Result.Let_syntax in
  let%map (size, f) = compile_e { orc_module; current_fun = ident;
                                  ctx = ctx'';
                                  tail_call = true;
                                  shift = 0;
                                  state} body in
  (size + List.length params + ctx_vars ctx', f)

let init_ctx = [("Coeffect", BindCoeffect)]

let finalize code =
  Array.of_list_map code ~f:(fun (_ident, (s, f)) ->
      (s, Array.of_list_rev f))

let compile ~repository ~prelude ~comments e =
  with_return (fun r ->
      let init_ctx' = init_ctx @ List.map prelude ~f:(fun (mod_, ident) ->
          (ident, BindMod(mod_, ident))) in
      let state = { repository = repository;
                    repo = [];
                    ffi_in_use = [];
                    compile_queue = [{orc_module = "";
                                      ident = "'main";
                                      is_closure = false;
                                      ctx = ref (Some init_ctx');
                                      params = [];
                                      body = e}]} in
      let compile_unit unit =
        (match compile_fun state unit with
         | Error _ as err -> r.return err
         | Ok(size, ops) ->
           let x = (unit.orc_module, unit.ident, size, Array.of_list_rev ops) in
           state.repo <- x::state.repo) in
      let remove_from_queue el =
        let rec f acc = function
          | [] -> (List.rev acc)
          | x::xs when compile_unit_equal x el ->
            (List.rev acc) @ xs
          | x::xs -> f (x::acc) xs in
        state.compile_queue <- f [] state.compile_queue in
      let rec compile_loop () =
        match state.compile_queue with
        | [] -> ()
        | unit::_ ->
          compile_unit unit;
          remove_from_queue unit;
          compile_loop () in
      compile_loop ();
      let code = Array.of_list_rev_map state.repo ~f:(fun (_, _, size, body) ->
          (size, body)) in
      Ok({I.ffi = List.rev state.ffi_in_use; code}))

let rec compile_module' ctx orc_module = function
  | Ir1.EModule ->
    List.filter_map ctx ~f:(function
        | (_, BindFun unit) -> Some unit
        | _ -> None)
  | Ir1.EFix(fs, (e, _)) ->
    let binds = List.map fs ~f:(fun (ident, params, body) ->
        (ident, BindFun {orc_module;
                         ident;
                         is_closure = false;
                         ctx = ref None;
                         params; body})) in
    let ctx' = binds @ ctx in
    List.iter binds ~f:(function | (_, BindFun { ctx }) -> ctx := Some ctx' | _ -> assert false);
    compile_module' ctx' orc_module e
  | ERefer(mod_', fs, (e, _)) ->
    let binds = (List.map fs ~f:(fun name ->
        (name, BindMod(mod_', name)))) in
    compile_module' (binds @ ctx) orc_module e
  | _ -> assert false

let compile_module ~repository ~name ~comments (e, _) =
  compile_module' init_ctx name e
  |> List.iter ~f:(Repository.set repository)
