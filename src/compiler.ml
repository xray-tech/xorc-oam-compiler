open Common

module I = Inter0

type unit = {
  is_closure : bool;
  ns : string;
  ident : string;
  ctx : ctx option ref;                (* ref here is only for recusrive defenitions. ctx of unit could contain unit itself *)
  params : string list;
  body : Ir1.e }
and binding =
  | BindVar
  | BindCoeffect
  | BindFun of unit
  | BindNs of (string * string)
and ctx = (string * binding) list [@@deriving sexp_of]

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
    | (_, BindFun _)::xs | (_,BindCoeffect)::xs | (_, BindNs(_))::xs ->
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
  | None -> Error (`UnboundVar(ident, pos))
  | Some(v) -> Ok v

let new_index = ctx_vars

let len = List.length

let compile_unit_equal a b =
  match (a, b) with
  | ({ns; ident}, {ns = ns'; ident = ident'})
    when String.equal ns' ns && String.equal ident' ident ->
    true
  | _ -> false

type state = {
  deps : unit list;
  mutable ffi_in_use : string list;
  mutable repo : (string * string * int * I.t array) list;
  mutable compile_queue : unit list;
}

let repo_index state ns ident =
  let rec f = function
    | [] -> None
    | (ns', ident', _, _)::xs when String.equal ns' ns && String.equal ident' ident ->
      Some(len xs)
    | _::xs -> f xs in
  f state.repo

let get_label state unit =
  let rec f i = function
    | [] ->
      state.compile_queue <- state.compile_queue @ [unit];
      len state.repo + i
    | {ns; ident}::_ when String.equal ident unit.ident ->
      len state.repo + i
    | _::xs -> f (i + 1) xs in
  match repo_index state "" unit.ident with
  | Some(i) -> i
  | None -> f 0 state.compile_queue

let get_import_label state (ns, ident) =
  with_return (fun r ->
      let rec f i = function
        | [] ->
          (match List.find state.deps ~f:(fun {ns = ns'; ident = ident'} ->
               String.equal ns' ns && String.equal ident' ident) with
           | None -> r.return None
           | Some(unit) ->
             state.compile_queue <- state.compile_queue @ [unit];
             len state.repo + i)
        | {ns = ns'; ident = ident'}::_ when String.equal ns' ns && String.equal ident' ident ->
          len state.repo + i
        | _::xs -> f (i+1) xs in
      match repo_index state ns ident with
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
  let rec step ctx (e, (_ast_e, _pos)) =
    let open! Ir1 in
    let if_out_of_scope x =
      match ctx_find' ctx x with
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
    | EConst _ | EStop | ENS -> [] in
  step (List.map init ~f:(fun ident -> (ident, BindVar))) e

let closure_vars ctx pos vars =
  with_return (fun r ->
      let filtered = List.filter vars ~f:(fun v ->
          match ctx_find ctx pos v with
          | Error(_) as err -> r.return err
          | Ok((_, BindVar)) -> true
          | _ -> false) in
      Ok filtered)

type env = {
  ns : string;
  current_fun : string;
  ctx : ctx;
  shift : int;
  tail_call : bool;
  state : state;
}

exception NS of ctx

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
                                    String.equal env.ns unit.ns ->
             let c = get_label env.state unit in
             (0, [I.TailCall(I.TFun(c), Array.of_list args')])
           | (_, BindFun unit) ->
             let c = get_label env.state unit in
             (0, [I.Call(I.TFun(c), Array.of_list args')])
           |  (_, BindNs f) ->
             let c = get_import_label' f in
             (0, [I.Call(I.TFun(c), Array.of_list args')])
           (* TODO tail call of closures *)
           | (i, BindVar) ->
             (0, [I.Call(I.TDynamic(i), Array.of_list args')])
           | (_, BindCoeffect) ->
             (match args' with
              | [i] -> (0, [I.Coeffect i])
              | _ -> raise Util.TODO)) in
        let need_preprocess args =
          List.find args ~f:(fun arg ->
              match ctx_find arg with
              | (_, BindFun(_))
              | (_, BindNs(_) | (_, BindCoeffect)) -> true
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
        | ENS -> raise (NS env.ctx)
        | EIdent x ->
          (* Stdio.eprintf "--LOOKUP %s: %s\n" x (sexp_of_ctx ctx |> Sexp.to_string_hum); *)
          (match ctx_find x with
           | (_, BindVar) ->
             (EFFI ("core.let", [x]), (ast_e, pos))
             |> compile_e' env
           | (_, BindFun unit) ->
             let c = get_label env.state unit in
             (0, [I.Label(c)])
           | (_, BindNs f) ->
             let c = get_import_label' f in
             (0, [I.Label(c)])
           | (_, BindCoeffect) -> raise Util.TODO)
        | EConst v -> (0, [I.Const v])
        | EParallel(e1, e2) ->
          let (s2, e2') = compile_e' env e2 in
          let (s1, e1') = compile_e' {env with shift = env.shift + len e2'} e1 in
          ((Int.max s1 s2),
           I.Parallel(env.shift + len e1' + len e2' - 1,
                      env.shift + len e2' - 1)
           ::(e1' @ e2'))
        | EOtherwise(e1, e2) ->
          let (s2, e2') = compile_e' env e2 in
          let (s1, e1') = compile_e' {env with shift = env.shift + len e2'} e1 in
          ((Int.max s1 s2),
           I.Otherwise(env.shift + len e1' + len e2' - 1,
                       env.shift + len e2' - 1)
           ::(e1' @ e2'))
        | EPruning(e1, v, e2) ->
          let (ctx', index) = match v with
            | None -> (env.ctx, None)
            | Some(v) -> ((v, BindVar)::env.ctx, Some(new_index env.ctx)) in
          (* FIXME ctx' in right branch? wrong *)
          let (s2, e2') = compile_e' { env with tail_call = false; ctx = ctx' } e2 in
          let (s1, e1') = compile_e' { env with shift = env.shift + len e2'; ctx = ctx'} e1 in
          ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
           I.Pruning(env.shift + len e1' + len e2' - 1,
                     index,
                     env.shift + len e2' -1)
           ::(e1' @ e2'))
        | ESequential(e1, v, e2) ->
          let (ctx', index) = match v with
            | None -> (env.ctx, None)
            | Some(v) -> ((v, BindVar)::env.ctx, Some(new_index env.ctx)) in
          let (s2, e2') = compile_e' { env with ctx = ctx' } e2 in
          let (s1, e1') = compile_e' { env with tail_call = false; shift = env.shift + len e2'} e1 in
          ((Int.max s1 s2) + (if Option.is_none index then 0 else 1),
           I.Sequential(env.shift + len e1' + len e2' - 1,
                        index,
                        env.shift + len e2' - 1)
           ::(e1' @ e2'))
        | EFix(fs, e) ->
          let vars = List.map fs ~f:(fun (ident, _, _) -> ident) in
          let closure_vars = List.map fs ~f:(fun (_, params, e) ->
              let all = free_vars (vars @ params) e in
              closure_vars env.ctx pos all) in
          let binds = List.map2_exn fs closure_vars ~f:(fun (ident, params, body) vars ->
              match vars with
              | Ok ([]) -> (ident, BindFun {ns = env.ns;
                                            ident;
                                            is_closure = false;
                                            ctx = ref None;
                                            params; body})
              | Ok _ -> (ident, BindVar)
              | Error _ as err -> r.return err ) in
          let ctx' = binds @ env.ctx in
          List.iter binds ~f:(function | (_, BindFun { ctx }) -> ctx := Some ctx' | _ -> ());
          let (s, body) = compile_e' { env with ctx = ctx'} e in
          let closure_size = ctx_vars ctx' in
          let make_fun (i, index, acc) (_, bind) (ident, params, body) =
            match bind with
            | BindFun _ ->
              (i, index, acc)
            | BindVar ->
              let c = get_label env.state {ns = env.ns;
                                           ident;
                                           is_closure = true;
                                           ctx = ref (Some ctx');
                                           params; body} in
              (i + 2, index + 1, I.Pruning(i - 1, Some(index), i)::I.Closure(c, closure_size)::acc)
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
              (name, BindNs(ns, name)))) @ env.ctx in
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
            (0, [I.FFI(get_ffi env.state def, Array.of_list args')])
        | EStop -> (0, [I.Stop]) in
      Ok(compile_e' env e))

let compile_fun state {ns; ident; is_closure; ctx; params; body} =
  let params' = List.rev_map params ~f:(fun n -> (n, BindVar)) in
  let ctx = Option.value_exn !ctx in
  let ctx' = if is_closure
    then ctx
    else List.filter ctx ~f:(function | (_, BindVar) -> false
                                      | _ -> true) in
  let ctx'' = params' @ ctx' in
  let open Result.Let_syntax in
  let%map (size, f) = compile_e { ns; current_fun = ident;
                                  ctx = ctx'';
                                  tail_call = true;
                                  shift = 0;
                                  state} body in
  (size + List.length params + ctx_vars ctx', f)

let rec prepare_dep ctx ns = function
  | Ir1.ENS ->
    List.filter_map ctx ~f:(function
        | (_, BindFun unit) -> Some unit
        | _ -> None)
  | Ir1.EFix(fs, (e, _)) ->
    let binds = List.map fs ~f:(fun (ident, params, body) ->
        (ident, BindFun {ns;
                         ident;
                         is_closure = false;
                         ctx = ref None;
                         params; body})) in
    let ctx' = binds @ ctx in
    List.iter binds ~f:(function | (_, BindFun { ctx }) -> ctx := Some ctx' | _ -> assert false);
    prepare_dep ctx' ns e
  | ERefer(ns', fs, (e, _)) ->
    let binds = (List.map fs ~f:(fun name ->
        (name, BindNs(ns', name)))) in
    prepare_dep (binds @ ctx) ns e
  | _ -> assert false

let init_ctx = [("Coeffect", BindCoeffect)]

let prepare_deps deps =
  List.concat_map deps ~f:(fun (ns, (e, _)) ->
      prepare_dep init_ctx ns e)

let finalize code =
  Array.of_list_map code ~f:(fun (_ident, (s, f)) ->
      (s, Array.of_list_rev f))

let compile ~deps e =
  with_return (fun r ->
      let state = { deps = prepare_deps deps;
                    repo = [];
                    ffi_in_use = [];
                    compile_queue = [{ns = "";
                                      ident = "'main";
                                      is_closure = false;
                                      ctx = ref (Some init_ctx);
                                      params = [];
                                      body = e}]} in
      let compile_unit unit =
        (match compile_fun state unit with
         | Error _ as err -> r.return err
         | Ok(size, ops) ->
           let x = (unit.ns, unit.ident, size, Array.of_list_rev ops) in
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
