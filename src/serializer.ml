open Common

type load_error =
  [ | `BadFormat ]

module M = Msgpck

let serialize_const = function
  | Ast.Int x -> M.Int x
  | Float x -> M.Float x
  | String x -> M.String x
  | Signal -> M.Ext(1, "")
  | Null -> M.Nil
  | Bool x -> M.Bool x

let deserialize_const = function
  | M.Int x -> Ok(Ast.Int x)
  | M.Float x -> Ok(Ast.Float x)
  | M.String x -> Ok(Ast.String x)
  | M.Ext(1,_) -> Ok(Ast.Signal)
  | M.Nil -> Ok(Ast.Null)
  | M.Bool x -> Ok(Ast.Bool x)
  | _ -> Error(`BadFormat)

let dump {Inter.ffc; code} =
  let array_to_list_map a ~f =
    Array.to_sequence a |> Sequence.map ~f |> Sequence.to_list in
  let obj = array_to_list_map code ~f:(fun (i, _, ecode) ->
      M.List ((M.Int i)::
              (M.Int (Array.length ecode))::
              (List.concat (array_to_list_map ecode ~f:(fun (op, _) ->
                   match op with
                   | Inter.Parallel(c1,c2) -> [M.Int 0; M.Int c1; M.Int c2]
                   | Inter.Otherwise(c1,c2) -> [M.Int 1; M.Int c1; M.Int c2]
                   | Inter.Pruning(c1,arg,c2) -> [M.Int 2;
                                                  M.Int c1;
                                                  (match arg with
                                                   | Some x -> M.Int (Inter.Var.index x)
                                                   | None -> M.Int(-1));
                                                  M.Int c2]
                   | Inter.Sequential(c1,arg,c2) -> [M.Int 3;
                                                     M.Int c1;
                                                     (match arg with
                                                      | Some x -> M.Int (Inter.Var.index x)
                                                      | None -> M.Int(-1));
                                                     M.Int c2]
                   | (Inter.Call(t, args) as x)
                   | (Inter.TailCall(t, args) as x) ->
                     (M.Int (match x with
                          | Inter.Call(_,_) -> 4
                          | Inter.TailCall(_, _) -> 5
                          | _ -> assert false))::
                     (match t with
                      | Inter.TFun i -> [M.Int 0; M.Int i]
                      | Inter.TDynamic i -> [M.Int 1; M.Int i]) @
                     [(M.List (array_to_list_map args ~f:(fun i -> M.Int i)))]
                   | Inter.Coeffect(i) -> [M.Int 6; M.Int i]
                   | Inter.Stop -> [M.Int 7]
                   | Inter.Const(v) -> [M.Int 8; serialize_const v]
                   | Inter.Closure(p, to_copy) -> [M.Int 9; M.Int p; M.Int to_copy]
                   | Inter.Label(p) -> [M.Int 10; M.Int p]
                   | Inter.FFC(target, args) ->
                     [M.Int 11;
                      M.Int target;
                      M.List (array_to_list_map args ~f:(fun i -> M.Int i))]
                 ))))) in
  let ffc' = M.List(List.map ffc ~f:(fun x -> M.String x)) in
  M.List [ffc'; M.List obj]

let load packed =
  with_return (fun r ->
      let bad_format () = r.return (Error `BadFormat) in
      let rec des_fun = function
        | [] -> []
        | ((M.Int 0)::(M.Int c1)::(M.Int c2)::xs) ->
          Inter.Parallel(c1, c2)::(des_fun xs)
        | ((M.Int 1)::(M.Int c1)::(M.Int c2)::xs) ->
          Inter.Otherwise(c1, c2)::(des_fun xs)
        | ((M.Int 2)::(M.Int c1)::(M.Int arg)::(M.Int c2)::xs) ->
          let arg' = (match arg with -1 -> None | x -> Some(Inter.Var.Generated x)) in
          Inter.Pruning(c1, arg', c2)::(des_fun xs)
        | ((M.Int 3)::(M.Int c1)::(M.Int arg)::(M.Int c2)::xs) ->
          let arg' = (match arg with -1 -> None | x -> Some(Inter.Var.Generated x)) in
          Inter.Sequential(c1, arg', c2)::(des_fun xs)
        | ((M.Int (4 as i))::(M.Int t)::(M.Int t_arg)::(M.List args)::xs)
        | ((M.Int (5 as i))::(M.Int t)::(M.Int t_arg)::(M.List args)::xs) ->
          let target = match t with
            | 0 -> Inter.TFun t_arg
            | 1 -> Inter.TDynamic t_arg
            |_ -> bad_format () in
          let args' = List.map args ~f:(function
              | M.Int x -> x
              | _ -> bad_format ())
                      |> Array.of_list in
          (match i with
           | 4 -> Inter.Call(target, args')
           | 5 -> Inter.TailCall(target, args')
           | _ -> bad_format ())::(des_fun xs)
        | ((M.Int 6)::(M.Int i)::xs) ->
          Inter.Coeffect(i)::(des_fun xs)
        | ((M.Int 7)::xs) ->
          Inter.Stop::(des_fun xs)
        | ((M.Int 8)::v::xs) ->
          (match (deserialize_const v) with
           | Error(_) as err -> r.return err
           | Ok(c) ->
             Inter.Const(c)::(des_fun xs))
        | ((M.Int 9)::(M.Int pc)::(M.Int to_copy)::xs) ->
          Inter.Closure(pc, to_copy)::(des_fun xs)
        | (M.Int 10)::(M.Int p)::xs ->
          Inter.Label(p)::(des_fun xs)
        | (M.Int 11)::(M.Int target)::(M.List args)::xs ->
          let args' = List.map args ~f:(function
              | M.Int x -> x
              | _ -> bad_format ())
                      |> Array.of_list in
          Inter.FFC(target, args')::(des_fun xs)

        | _ -> bad_format () in
      match packed with
      | M.List[M.List ffc_raw; M.List code_raw] ->
        let ffc = List.map ffc_raw ~f:(function
            | M.String s -> s
            | _ -> bad_format ()) in
        let code = Array.of_list (List.map code_raw ~f:(function
            | M.List ((M.Int i)::(M.Int _ops)::xs) ->
              (i, [], Array.of_list (List.map (des_fun xs) ~f:(fun op -> (op, Ast.Pos.dummy))))
            | _ -> bad_format ())) in
        Ok({Inter.ffc; code})
      | _ -> bad_format ())

open Inter

let rec dump_simple_value = function
  | VConst x ->
    [M.Int 0; serialize_const x]
  | VTuple vs ->
    [M.Int 2; M.List (List.concat_map vs ~f:(dump_simple_value))]
  | VList vs ->
    [M.Int 3; M.List (List.concat_map vs ~f:(dump_simple_value))]
  | VRecord pairs ->
    [M.Int 4; M.List (List.concat_map pairs ~f:(fun (k, v) ->
         (M.String k)::(dump_simple_value v)))]
  | VLabel pc ->
    [M.Int 5; M.Int pc]
  | VRef v ->
    dump_simple_value !v
  | _ -> assert false

let load_simple_value v =
  with_return (fun r ->
      let rec f = function
        | (M.Int 0)::v::xs ->
          (match deserialize_const v with
           | Ok(c) -> (VConst(c), xs)
           | Error(_) as err -> r.return err)
        | (M.Int 2)::(M.List vs)::xs ->
          (VTuple(multiple vs), xs)
        | (M.Int 3)::(M.List vs)::xs ->
          (VList(multiple vs), xs)
        | (M.Int 4)::(M.List pairs)::xs ->
          let rec deserialize_pairs = function
            | [] -> []
            | (M.String field)::xs ->
              let (v, xs') = f xs in
              (field, v)::(deserialize_pairs xs')
            | _ -> r.return (Error `BadFormat) in
          (VRecord(deserialize_pairs pairs), xs)
        | (M.Int 5)::(M.Int pc)::xs ->
          (VLabel(pc), xs)
        | _ -> r.return (Error `BadFormat)
      and multiple = function
        | [] -> []
        | xs -> let (v, xs') = f xs in
          v::(multiple xs') in
      Ok(f v))

let dump_instance { current_coeffect; blocks } =
  let id = ref 0 in
  let make_id () = id := !id + 1; !id in
  let frames = ref [] in
  let envs = ref [] in
  let pendings = ref [] in
  let refs = ref [] in
  let in_cache cache obj =
    match List.find_map !cache ~f:(function
        | (id, obj') when phys_equal obj obj' -> Some(id)
        | _ -> None) with
    | Some id ->
      `Exists id
    | None ->
      let id = make_id () in
      cache := (id, obj)::!cache;
      `New id in
  let dedup cache obj =
    match in_cache cache obj with
    | `Exists id | `New id -> M.Int id in
  let on_new cache obj f =
    match in_cache cache obj with
    | `Exists _ -> ()
    | `New _ -> f () in
  let rec dump_value = function
    | VConst x ->
      [M.Int 0; serialize_const x]
    | VClosure(pc, to_copy, env) ->
      [M.Int 1; M.Int pc; M.Int to_copy; dedup envs env]
    | VTuple vs ->
      [M.Int 2; M.List (List.concat_map vs ~f:dump_value)]
    | VList vs ->
      [M.Int 3; M.List (List.concat_map vs ~f:dump_value)]
    | VRecord pairs ->
      [M.Int 4; M.List (List.concat_map pairs ~f:(fun (k, v) ->
           (M.String k)::(dump_value v)))]
    | VLabel pc ->
      [M.Int 5; M.Int pc]
    | VRef v ->
      [(M.Int 6); (dedup refs v)]
    | VPending p ->
      [(M.Int 7); (dedup pendings p)] in
  let serialize_frame (id, frame) =
    let f = match frame with
      | FPruning { instances; pending } ->
        [M.Int 0; M.Int instances; dedup pendings pending]
      | FOtherwise { first_value; instances; op = (pc, c) } ->
        [M.Int 1; M.Bool first_value; M.Int instances; M.Int pc; M.Int c]
      | FSequential(var, (pc, c)) ->
        [M.Int 2;
         M.Int (Option.map var ~f:Inter.Var.index |> Option.value ~default: (-1));
         M.Int pc; M.Int c]
      | FCall(env) ->
        [M.Int 3; dedup envs env]
      | FResult -> [M.Int 4] in
    (M.Int id)::f in
  let rec serialize_pending (id, { pend_value; pend_waiters }) =
    let tokens = List.map pend_waiters ~f:serialize_token in
    let v = match pend_value with
      | Pend -> [M.Int 0; M.List tokens]
      | PendVal v -> (M.Int 1)::(dump_value v)
      | PendStopped -> [M.Int 2] in
    (M.Int id)::v
  and serialize_env_v = function
    | (_, Value x) -> M.List ((M.Int 0)::(dump_value x))
    | (_, Pending x) -> M.List [M.Int 1; dedup pendings x]
  and serialize_env (id, env) =
    [M.Int id; M.List (Array.map env ~f:serialize_env_v |> Array.to_list)]
  and serialize_token { op = (pc, c); stack; env } =
    M.List [M.Int pc;
            M.Int c;
            M.List (List.map stack ~f:(dedup frames));
            dedup envs env] in
  let serialize_ref (i, v) =
    (M.Int i)::dump_value !v in
  let serialize_block (i, token) =
    [M.Int i; serialize_token token] in
  let rec walk_token { stack; env } =
    walk_stack stack;
    walk_env env
  and walk_pending ({ pend_value; pend_waiters } as p) =
    on_new pendings p (fun () ->
        (match pend_value with
         | PendVal v -> walk_v v
         | _ -> ());
        List.iter pend_waiters ~f:walk_token)
  and walk_v = function
    | VClosure(_,_,env) -> walk_env env
    | VPending(pending) -> walk_pending pending
    | _ -> ()
  and walk_env env =
    on_new envs env (fun () ->
        Array.iter env ~f:(function
            | (_, Value v) -> walk_v v
            | (_, Pending p) -> walk_pending p))
  and walk_frame frame =
    on_new frames frame (fun () ->
        match frame with
        | FPruning { pending } -> walk_pending pending
        | FCall(env) -> walk_env env
        | _ -> ())
  and walk_stack l = List.iter l ~f: walk_frame in
  List.iter blocks ~f:(fun (_id, token) -> walk_token token);
  M.List [(M.Int current_coeffect);
          M.List (List.concat_map !frames ~f:serialize_frame);
          M.List (List.concat_map !pendings ~f:serialize_pending);
          M.List (List.concat_map !refs ~f:serialize_ref);
          M.List (List.concat_map !envs ~f:serialize_env);
          M.List (List.concat_map blocks ~f:serialize_block)]

let serialize_result {Inter.Res.coeffects; killed; values} =
  let values' = (List.concat_map values ~f:dump_simple_value) in
  let serialize_coeffect (id, v) =
    M.List ((M.Int id)::(dump_simple_value v)) in
  M.List
    [M.List values';
     M.List (List.map coeffects ~f:serialize_coeffect);
     M.List (List.map killed ~f:(fun i -> M.Int i))]
  |> Msgpck.String.to_string

let load_instance packed =
  with_return (fun r ->
      let bad_format () = r.return (Error `BadFormat) in
      let frames_repo = ref [] in
      let envs_repo = ref [] in
      let pendings_repo = ref [] in
      let refs_repo = ref [] in
      let repo_find repo id =
        List.Assoc.find !repo ~equal:Int.equal id in
      let add_repo repo id v = repo := (id, v)::!repo in
      let dummy_pending () = { pend_value = Pend; pend_waiters = [] } in
      let repo_or_dummy_pending id =
        match repo_find pendings_repo id with
        | Some(v) -> v
        | None -> let p = dummy_pending () in
          add_repo pendings_repo id p;
          p in
      let repo_or_dummy_ref id =
        match repo_find refs_repo id with
        | Some(v) -> v
        | None -> let v = ref (VConst Ast.Null) in
          add_repo refs_repo id v;
          v in
      let dummy_env len = Array.create ~len (Var.dummy, Value (VConst Ast.Null)) in
      match packed with
      | M.List [M.Int current_coeffect;
                M.List frames;
                M.List pendings;
                M.List refs;
                M.List envs;
                M.List blocks] ->
        let env_size id =
          let rec step = function
            | [] -> bad_format ()
            | (M.Int id')::(M.List l)::xs ->
              if Int.equal id id' then (List.length l) else step xs
            | _ -> bad_format () in
          step envs in
        let repo_or_dummy_env id =
          match repo_find envs_repo id with
          | Some(v) -> v
          | None -> let e = dummy_env (env_size id) in
            add_repo envs_repo id e;
            e in
        let load_value v =
          let rec f = function
            | (M.Int 0)::v::xs ->
              (match deserialize_const v with
               | Ok(c) -> (VConst(c), xs)
               | Error(_) as err -> r.return err)
            | (M.Int 1)::(M.Int pc)::(M.Int to_copy)::(M.Int env)::xs ->
              (VClosure(pc, to_copy, repo_or_dummy_env env), xs)
            | (M.Int 2)::(M.List vs)::xs ->
              (VTuple(multiple vs), xs)
            | (M.Int 3)::(M.List vs)::xs ->
              (VList(multiple vs), xs)
            | (M.Int 4)::(M.List pairs)::xs ->
              let rec deserialize_pairs = function
                | [] -> []
                | (M.String field)::xs ->
                  let (v, xs') = f xs in
                  (field, v)::(deserialize_pairs xs')
                | _ -> r.return (Error `BadFormat) in
              (VRecord(deserialize_pairs pairs), xs)
            | (M.Int 5)::(M.Int pc)::xs ->
              (VLabel(pc), xs)
            | (M.Int 6)::(M.Int reference)::xs ->
              (VRef(repo_or_dummy_ref reference), xs)
            | (M.Int 7)::(M.Int pending)::xs ->
              (VPending(repo_or_dummy_pending pending), xs)
            | _ -> r.return (Error `BadFormat)

          and multiple = function
            | [] -> []
            | xs -> let (v, xs') = f xs in
              v::(multiple xs') in
          Ok(f v) in
        let rec deserialize_frames = function
          | [] -> ()
          | (M.Int id)::(M.Int 0)::(M.Int instances)::(M.Int pending)::xs ->
            add_repo frames_repo id (FPruning { instances; pending = repo_or_dummy_pending pending});
            deserialize_frames xs
          | (M.Int id)::(M.Int 1)::(M.Bool first_value)::(M.Int instances)::(M.Int pc)::(M.Int c)::xs ->
            add_repo frames_repo id (FOtherwise { first_value; instances; op = (pc, c) });
            deserialize_frames xs
          | (M.Int id)::(M.Int 2)::(M.Int i)::(M.Int pc)::(M.Int c)::xs ->
            let i' = if Int.equal (-1) i then None else Some(Var.Generated i) in
            add_repo frames_repo id (FSequential(i', (pc, c)));
            deserialize_frames xs
          | (M.Int id)::(M.Int 3)::(M.Int env)::xs ->
            add_repo frames_repo id (FCall(repo_or_dummy_env env));
            deserialize_frames xs
          | (M.Int id)::(M.Int 4)::xs ->
            add_repo frames_repo id FResult;
            deserialize_frames xs
          | _ -> bad_format () in
        let deserialize_thread = function
          | M.List [M.Int pc; M.Int c; M.List frames; M.Int env] ->
            let stack = List.map frames ~f:(function
                | M.Int id -> repo_find frames_repo id |> Option.value_exn
                | _ -> bad_format ()) in
            { op = (pc, c);
              stack;
              env = repo_or_dummy_env env;
              id = -1;
              pos = Ast.Pos.dummy}
          | _ -> bad_format () in
        let deserialize_tokens = List.map ~f:deserialize_thread in
        let rec deserialize_pendings = function
          | [] -> ()
          | (M.Int id)::xs ->
            let (v, tokens, xs') = match xs with
              | (M.Int 0)::(M.List tokens)::xs' ->
                (Pend, deserialize_tokens tokens, xs')
              | (M.Int 1)::xs' ->
                (match load_value xs' with
                 | Error(_) as err -> r.return err
                 | Ok((v, xs'')) ->
                   (PendVal v, [], xs''))
              | (M.Int 2)::_ -> (PendStopped, [], xs)
              | _ -> bad_format () in
            let p = repo_or_dummy_pending id in
            p.pend_value <- v;
            p.pend_waiters <- tokens;
            deserialize_pendings xs'
          | _ -> bad_format () in
        let deserialize_env_value = function
          | M.List ((M.Int 0)::xs) ->
            (match load_value xs with
             | Error _ as err -> r.return err
             | Ok((v, _)) ->
               (Var.dummy, Value v))
          | M.List [M.Int 1; M.Int pending] ->
            (Var.dummy, Pending (repo_or_dummy_pending pending))
          | _ -> bad_format () in
        let rec deserialize_envs = function
          | [] -> ()
          | (M.Int id)::(M.List vals)::xs ->
            let e = repo_or_dummy_env id in
            List.iteri vals ~f:(fun i v ->
                e.(i) <- deserialize_env_value v);
            deserialize_envs xs
          | _ -> bad_format () in
        let rec deserialize_refs = function
          | [] -> ()
          | (M.Int id)::xs ->
            (match load_value xs with
             | Error _ as err -> r.return err
             | Ok((v, xs')) ->
               let x = repo_or_dummy_ref id in
               x := v;
               deserialize_refs xs')
          | _ -> bad_format () in
        let rec deserialize_blocks = function
          | [] -> []
          | (M.Int id)::t::xs ->
            (id, deserialize_thread t)::(deserialize_blocks xs)
          | _ -> bad_format () in
        deserialize_frames frames;
        deserialize_pendings pendings;
        deserialize_envs envs;
        deserialize_refs refs;
        let blocks' = deserialize_blocks blocks in
        Ok({ current_coeffect; blocks = blocks' })
      | _ -> bad_format ()
    )

let load_no_linker v =
  match load v with
  | Ok(_) as ok -> ok
  | Error(`BadFormat) as err -> err
  | Error(`LinkerError _) -> assert false

let dump_k {Inter.ffc; code} =
  let open Printf in
  let k_ffc = List.mapi ffc ~f:(fun i def -> sprintf "  %i \"%s\"" i def)
              |> String.concat ~sep:"\n" in
  let k_op' = function
    | Parallel(left, right) -> sprintf "#parallel %i %i" left right
    | Otherwise(left, right) -> sprintf "#otherwise %i %i" left right
    | Sequential(left, None, right) -> sprintf "#sequential %i -1 %i" left right
    | Sequential(left, Some(param), right) -> sprintf "#sequential %i %i %i" left (Inter.Var.index param) right
    | Pruning(left, None, right) -> sprintf "#pruning %i -1 %i" left right
    | Pruning(left, Some(param), right) -> sprintf "#pruning %i %i %i" left (Inter.Var.index param) right
    | Call(target, args) | TailCall(target, args) ->
      let target' = match target with
        | TFun(i) -> sprintf "#callFun %i" i
        | TDynamic(i) -> sprintf "#callDynamic %i" i in
      let args' = Array.map args ~f:Int.to_string
                  |> Array.to_list
                  |> String.concat ~sep:", " in
      sprintf "%s [ %s ]" target' args'
    | FFC(target, args) ->
      let args' = Array.map args ~f:Int.to_string
                  |> Array.to_list
                  |> String.concat ~sep:", " in
      sprintf "#ffc %i [ %s ]" target args'
    | Stop -> "#stop"
    | Const(Ast.Int i) -> sprintf "#constInt %i" i
    | Const(Ast.Bool v) -> sprintf "#constBool %b" v
    | Const(Ast.String v) -> sprintf "#constString \"%s\"" v
    | Const(Ast.Signal) -> sprintf "#constSignal"
    | Closure(pc, to_copy) -> sprintf "#closure %i %i" pc to_copy
    | Coeffect(arg) -> sprintf "#coeffect %i" arg
    | _ -> "" in
  let k_op i (op, _) =
    sprintf "  %i: %s" i (k_op' op) in
  let k_fun i (_, _, ops) =
    let k_ops = Array.mapi ops ~f:k_op
                |> Array.to_list
                |> String.concat ~sep:"\n" in
    sprintf "%i %i {\n%s\n}" i (Array.length ops - 1) k_ops in
  let k_funs = Array.mapi code ~f:k_fun
               |> Array.to_list
               |> String.concat ~sep:"\n\n" in
  sprintf "ffc {\n%s\n}\n\n%s\n" k_ffc k_funs
