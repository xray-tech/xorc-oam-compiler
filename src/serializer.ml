open Common

module M = Msgpck

exception BadFormat

let serialize_const = function
  | Ast.Int x -> M.Int x
  | Float x -> M.Float x
  | String x -> M.String x
  | Signal -> M.Ext(1, "")
  | Null -> M.Nil
  | Bool x -> M.Bool x

let deserialize_const = function
  | M.Int x -> Ast.Int x
  | M.Float x -> Ast.Float x
  | M.String x -> Ast.String x
  | M.Ext(1,_) -> Ast.Signal
  | M.Nil -> Ast.Null
  | M.Bool x -> Ast.Bool x
  | _ -> raise BadFormat

let dump ?(imports = []) code =
  let array_to_list_map a ~f =
    Array.to_sequence a |> Sequence.map ~f |> Sequence.to_list in
  let obj = array_to_list_map code ~f:(fun (i, ecode) ->
      M.List ((M.Int i)::
              (M.Int (Array.length ecode))::
              (List.concat (array_to_list_map ecode ~f:(function
                   | Inter.Parallel(c1,c2) -> [M.Int 0; M.Int c1; M.Int c2]
                   | Inter.Otherwise(c1,c2) -> [M.Int 1; M.Int c1; M.Int c2]
                   | Inter.Pruning(c1,arg,c2) -> [M.Int 2;
                                                  M.Int c1;
                                                  (match arg with
                                                   | Some x -> M.Int x
                                                   | None -> M.Int(-1));
                                                  M.Int c2]
                   | Inter.Sequential(c1,arg,c2) -> [M.Int 3;
                                                     M.Int c1;
                                                     (match arg with
                                                      | Some x -> M.Int x
                                                      | None -> M.Int(-1));
                                                     M.Int c2]
                   | (Inter.Call(t, args) as x)
                   | (Inter.TailCall(t, args) as x) ->
                     (M.Int (match x with
                          | Inter.Call(_,_) -> 4
                          | Inter.TailCall(_, _) -> 5
                          | _ -> assert false))::
                     (match t with
                      | Inter.TPrim i -> [M.Int 0; M.Int i]
                      | Inter.TFun i -> [M.Int 1; M.Int i]
                      | Inter.TDynamic i -> [M.Int 2; M.Int i]) @
                     [(M.List (array_to_list_map args ~f:(fun i -> M.Int i)))]
                   | Inter.Coeffect(i) -> [M.Int 6; M.Int i]
                   | Inter.Stop -> [M.Int 7]
                   | Inter.Const(v) -> [M.Int 8; serialize_const v]
                   | Inter.Closure(p, to_copy) -> [M.Int 9; M.Int p; M.Int to_copy]
                   | Inter.Label(p) -> [M.Int 10; M.Int p]
                   | Inter.Prim(p) -> [M.Int 11; M.Int p]
                 ))))) in
  let rec dump_imports = function
    | [] -> []
    | (i, {Compiler.Fun.ns; name})::xs ->
      (M.Int i)::(M.String ns)::(M.String name)::(dump_imports xs) in
  let imports' = M.List(dump_imports imports) in
  M.List [imports'; M.List obj]

let load' linker packed =
  let linker' p = Or_error.ok_exn (linker p) in
  let rec des_fun = function
    | [] -> []
    | ((M.Int 0)::(M.Int c1)::(M.Int c2)::xs) ->
      Inter.Parallel(c1, c2)::(des_fun xs)
    | ((M.Int 1)::(M.Int c1)::(M.Int c2)::xs) ->
      Inter.Otherwise(c1, c2)::(des_fun xs)
    | ((M.Int 2)::(M.Int c1)::(M.Int arg)::(M.Int c2)::xs) ->
      let arg' = (match arg with -1 -> None | x -> Some(x)) in
      Inter.Pruning(c1, arg', c2)::(des_fun xs)
    | ((M.Int 3)::(M.Int c1)::(M.Int arg)::(M.Int c2)::xs) ->
      let arg' = (match arg with -1 -> None | x -> Some(x)) in
      Inter.Sequential(c1, arg', c2)::(des_fun xs)
    | ((M.Int (4 as i))::(M.Int t)::(M.Int t_arg)::(M.List args)::xs)
    | ((M.Int (5 as i))::(M.Int t)::(M.Int t_arg)::(M.List args)::xs) ->
      let target = match t with
        | 0 -> Inter.TPrim t_arg
        | 1 -> Inter.TFun (linker' t_arg)
        | 2 -> Inter.TDynamic t_arg
        |_ -> raise BadFormat in
      let args' = List.map args ~f:(function
          | M.Int x -> x
          | _ -> raise BadFormat)
                  |> Array.of_list in
      (match i with
       | 4 -> Inter.Call(target, args')
       | 5 -> Inter.TailCall(target, args')
       | _ -> raise BadFormat)::(des_fun xs)
    | ((M.Int 6)::(M.Int i)::xs) ->
      Inter.Coeffect(i)::(des_fun xs)
    | ((M.Int 7)::xs) ->
      Inter.Stop::(des_fun xs)
    | ((M.Int 8)::v::xs) ->
      Inter.Const(deserialize_const v)::(des_fun xs)
    | ((M.Int 9)::(M.Int pc)::(M.Int to_copy)::xs) ->
      Inter.Closure((linker' pc), to_copy)::(des_fun xs)
    | (M.Int 10)::(M.Int p)::xs ->
      Inter.Label(linker' p)::(des_fun xs)
    | (M.Int 11)::(M.Int p)::xs ->
      Inter.Prim(p)::(des_fun xs)
    | _ -> raise BadFormat in
  match packed with
  | M.List[_; (M.List xs)] ->
    Array.of_list (List.map xs ~f:(function
        | M.List ((M.Int i)::(M.Int _ops)::xs) -> (i, Array.of_list (des_fun xs))
        | _ -> raise BadFormat))
  | _ -> raise BadFormat

let imports packed =
  let err = Or_error.of_exn BadFormat in
  with_return (fun r ->
      let rec f = function
        | [] -> []
        | (M.Int i)::(M.String ns)::(M.String name)::xs ->
          (i, {Compiler.Fun.ns; name})::(f xs)
        | _ -> r.return err in
      match packed with
      | M.List((M.List xs)::_) ->
        Ok(f xs)
      | _ -> r.return err)

let load ?(linker=Or_error.return) v = Or_error.try_with ~backtrace:true (fun () -> load' linker v)

open Inter

let rec dump_value on_env = function
  | VConst x ->
    [M.Int 0; serialize_const x]
  | VClosure(pc, to_copy, env) ->
    [M.Int 1; M.Int pc; M.Int to_copy; on_env env]
  | VTuple vs ->
    [M.Int 2; M.List (List.concat_map vs ~f:(dump_value on_env))]
  | VList vs ->
    [M.Int 3; M.List (List.concat_map vs ~f:(dump_value on_env))]
  | VRecord pairs ->
    [M.Int 4; M.List (List.concat_map pairs ~f:(fun (k, v) ->
         (M.String k)::(dump_value on_env v)))]
  | VLabel pc ->
    [M.Int 5; M.Int pc]
  | VPrim pc ->
    [M.Int 6; M.Int pc]


let dump_instance ?imports { current_coeffect; blocks } =
  let _imports = imports in
  let id = ref 0 in
  let make_id () = id := !id + 1; !id in
  let frames = ref [] in
  let envs = ref [] in
  let pendings = ref [] in
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
  let serialize_frame (id, frame) =
    let f = match frame with
      | FPruning { instances; pending } ->
        [M.Int 0; M.Int instances; dedup pendings pending]
      | FOtherwise { first_value; instances; pc = (pc, c) } ->
        [M.Int 1; M.Bool first_value; M.Int instances; M.Int pc; M.Int c]
      | FSequential(i, (pc, c)) ->
        [M.Int 2;
         M.Int (Option.value i ~default: (-1));
         M.Int pc; M.Int c]
      | FCall(env) ->
        [M.Int 3; dedup envs env]
      | FResult -> [M.Int 4] in
    (M.Int id)::f in
  let rec serialize_pending (id, { pend_value; pend_waiters }) =
    let tokens = List.map pend_waiters ~f:serialize_token in
    let v = match pend_value with
      | Pend -> [M.Int 0; M.List tokens]
      | PendVal v -> (M.Int 1)::(dump_value (dedup envs) v)
      | PendStopped -> [M.Int 2] in
    (M.Int id)::v
  and serialize_env_v = function
    | Value x -> M.List ((M.Int 0)::(dump_value (dedup envs) x))
    | Pending x -> M.List [M.Int 1; dedup pendings x]
  and serialize_env (id, env) =
    [M.Int id; M.List (Array.map env ~f:serialize_env_v |> Array.to_list)]
  and serialize_token { pc = (pc, c); stack; env } =
    M.List [M.Int pc;
            M.Int c;
            M.List (List.map stack ~f:(dedup frames));
            dedup envs env] in
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
    | _ -> ()
  and walk_env env =
    on_new envs env (fun () ->
        Array.iter env ~f:(function
            | Value v -> walk_v v
            | Pending p -> walk_pending p))
  and walk_frame frame =
    on_new frames frame (fun () ->
        match frame with
        | FPruning { pending } -> walk_pending pending
        | FCall(env) -> walk_env env
        | _ -> ())
  and walk_stack = List.iter ~f: walk_frame in
  List.iter blocks ~f:(fun (_id, token) -> walk_token token);
  M.List [(M.Int current_coeffect);
          M.List (List.concat_map !frames ~f:serialize_frame);
          M.List (List.concat_map !pendings ~f:serialize_pending);
          M.List (List.concat_map !envs ~f:serialize_env);
          M.List (List.concat_map blocks ~f:serialize_block)]

let serialize_result {Inter.Res.coeffects; killed; values} =
  let serialize_value = (dump_value (fun _ -> assert false)) in
  let values' = (List.concat_map values ~f:serialize_value) in
  let serialize_coeffect (id, v) =
    M.List ((M.Int id)::(serialize_value v)) in
  M.List
    [M.List values';
     M.List (List.map coeffects ~f:serialize_coeffect);
     M.List (List.map killed ~f:(fun i -> M.Int i))]
  |> Msgpck.String.to_string

let rec load_value' on_env = function
  | (M.Int 0)::v::xs -> (VConst(deserialize_const v), xs)
  | (M.Int 1)::(M.Int pc)::(M.Int to_copy)::(M.Int env)::xs ->
    (VClosure(pc, to_copy, on_env env), xs)
  | (M.Int 2)::(M.List vs)::xs ->
    (VTuple(load_values on_env vs), xs)
  | (M.Int 3)::(M.List vs)::xs ->
    (VList(load_values on_env vs), xs)
  | (M.Int 4)::(M.List pairs)::xs ->
    let rec deserialize_pairs = function
      | [] -> []
      | (M.String f)::xs ->
        let (v, xs') = load_value' on_env xs in
        (f, v)::(deserialize_pairs xs')
      | _ -> raise BadFormat in
    (VRecord(deserialize_pairs pairs), xs)
  | (M.Int 5)::(M.Int pc)::xs ->
    (VLabel(pc), xs)
  | (M.Int 6)::(M.Int pc)::xs ->
    (VPrim(pc), xs)
  | _ -> raise BadFormat

and load_value on_env v =
  Or_error.try_with ~backtrace:true (fun () -> load_value' on_env v)

and load_values on_env = function
  | [] -> []
  | xs -> let (v, xs') = load_value' on_env xs in
    v::(load_values on_env xs')

let load_instance' packed =
  let frames_repo = ref [] in
  let envs_repo = ref [] in
  let pendings_repo = ref [] in
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
  let dummy_env len = Array.create ~len (Value (VConst (Ast.Null))) in
  match packed with
  | M.List [M.Int current_coeffect;
            M.List frames;
            M.List pendings;
            M.List envs;
            M.List blocks] ->
    let env_size id =
      let rec step = function
        | [] -> raise BadFormat
        | (M.Int id')::(M.List l)::xs ->
          if Int.equal id id' then (List.length l) else step xs
        | _ -> raise BadFormat in
      step envs in
    let repo_or_dummy_env id =
      match repo_find envs_repo id with
      | Some(v) -> v
      | None -> let e = dummy_env (env_size id) in
        add_repo envs_repo id e;
        e in
    let rec deserialize_frames = function
      | [] -> ()
      | (M.Int id)::(M.Int 0)::(M.Int instances)::(M.Int pending)::xs ->
        add_repo frames_repo id (FPruning { instances; pending = repo_or_dummy_pending pending});
        deserialize_frames xs
      | (M.Int id)::(M.Int 1)::(M.Bool first_value)::(M.Int instances)::(M.Int pc)::(M.Int c)::xs ->
        add_repo frames_repo id (FOtherwise { first_value; instances; pc = (pc, c) });
        deserialize_frames xs
      | (M.Int id)::(M.Int 2)::(M.Int i)::(M.Int pc)::(M.Int c)::xs ->
        let i' = if Int.equal (-1) i then None else Some(i) in
        add_repo frames_repo id (FSequential(i', (pc, c)));
        deserialize_frames xs
      | (M.Int id)::(M.Int 3)::(M.Int env)::xs ->
        add_repo frames_repo id (FCall(repo_or_dummy_env env));
        deserialize_frames xs
      | (M.Int id)::(M.Int 4)::xs ->
        add_repo frames_repo id FResult;
        deserialize_frames xs
      | _ -> raise BadFormat in
    let deserialize_token = function
      | M.List [M.Int pc; M.Int c; M.List frames; M.Int env] ->
        let stack = List.map frames ~f:(function
            | M.Int id -> repo_find frames_repo id |> Option.value_exn
            | _ -> raise BadFormat) in
        { pc = (pc, c); stack; env = repo_or_dummy_env env}
      | _ -> raise BadFormat in
    let deserialize_tokens = List.map ~f:deserialize_token in
    let rec deserialize_pendings = function
      | [] -> ()
      | (M.Int id)::xs ->
        let (v, tokens, xs') = match xs with
          | (M.Int 0)::(M.List tokens)::xs' ->
            (Pend, deserialize_tokens tokens, xs')
          | (M.Int 1)::xs' ->
            let (v, xs'') = load_value' repo_or_dummy_env xs' in
            (PendVal v, [], xs'')
          | (M.Int 2)::_ -> (PendStopped, [], xs)
          | _ -> raise BadFormat in
        let p = repo_or_dummy_pending id in
        p.pend_value <- v;
        p.pend_waiters <- tokens;
        deserialize_pendings xs'
      | _ -> raise BadFormat in
    let deserialize_env_value = function
      | M.List ((M.Int 0)::xs) ->
        let (v, _) = load_value' repo_or_dummy_env xs in
        Value v
      | M.List [M.Int 1; M.Int pending] ->
        Pending (repo_or_dummy_pending pending)
      | _ -> raise BadFormat in
    let rec deserialize_envs = function
      | [] -> ()
      | (M.Int id)::(M.List vals)::xs ->
        let e = repo_or_dummy_env id in
        List.iteri vals ~f:(fun i v ->
            e.(i) <- deserialize_env_value v);
        deserialize_envs xs
      | _ -> raise BadFormat in
    let rec deserialize_blocks = function
      | [] -> []
      | (M.Int id)::t::xs ->
        (id, deserialize_token t)::(deserialize_blocks xs)
      | _ -> raise BadFormat in
    deserialize_frames frames;
    deserialize_pendings pendings;
    deserialize_envs envs;
    let blocks' = deserialize_blocks blocks in
    { current_coeffect; blocks = blocks' }
  | _ -> raise BadFormat

let load_instance ?linker v =
  let _linker = linker in
  Or_error.try_with ~backtrace:true (fun () -> load_instance' v)
