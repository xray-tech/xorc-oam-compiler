open Base

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

let serialize_bc code =
  let array_to_list_map a ~f =
    Array.to_sequence a |> Sequence.map ~f |> Sequence.to_list in
  let obj = array_to_list_map code (fun (i, ecode) ->
      M.List ((M.Int i)::
              (M.Int (Array.length ecode))::
              (List.concat (array_to_list_map ecode (function
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
                      | Inter.TClosure i -> [M.Int 2; M.Int i]) @
                     [(M.List (array_to_list_map args (fun i -> M.Int i)))]
                   | Inter.Coeffect(i) -> [M.Int 6; M.Int i]
                   | Inter.Stop -> [M.Int 7]
                   | Inter.Const(v) -> [M.Int 8; serialize_const v]
                   | Inter.Closure(p, to_copy) -> [M.Int 9; M.Int p; M.Int to_copy]
                 ))))) in
  Msgpck.String.to_string (M.List obj)

let deserialize_bc s =
  let module M = Msgpck in
  let (_, packed) = M.String.read s in
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
        | 1 -> Inter.TFun t_arg
        | 2 -> Inter.TClosure t_arg
        |_ -> raise BadFormat in
      let args' = List.map args (function
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
      Inter.Closure(pc, to_copy)::(des_fun xs)
    | _ -> raise BadFormat in
  match packed with
  | M.List xs -> Array.of_list (List.map xs (function
      | M.List ((M.Int i)::(M.Int _ops)::xs) -> (i, Array.of_list @@ des_fun xs)
      | _ -> raise BadFormat))
  | _ -> raise BadFormat

open Inter
let serialize { current_coeffect; blocks } =
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
    let tokens = List.map pend_waiters serialize_token in
    let v = match pend_value with
      | Pend -> [M.Int 0; M.List tokens]
      | PendVal v -> (M.Int 1)::(serialize_value v)
      | PendStopped -> [M.Int 2] in
    (M.Int id)::v
  and serialize_value = function
    | VConst x ->
      [M.Int 0; serialize_const x]
    | VClosure(pc, to_copy, env) ->
      [M.Int 1; M.Int pc; M.Int to_copy; dedup envs env]
  and serialize_env_v = function
    | Value x -> M.List ((M.Int 0)::(serialize_value x))
    | Pending x -> M.List [M.Int 1; dedup pendings x]
  and serialize_env (id, env) =
    [M.Int id; M.List (Array.map env serialize_env_v |> Array.to_list)]
  and serialize_token { pc = (pc, c); stack; env } =
    M.List [M.Int pc;
            M.Int c;
            M.List (List.map stack (dedup frames));
            dedup envs env] in
  let serialize_block (i, token) = [M.Int i; serialize_token token] in
  let rec walk_token { stack; env } =
    walk_stack stack;
    walk_env env
  and walk_pending ({ pend_value; pend_waiters } as p) =
    on_new pendings p (fun () ->
        (match pend_value with
         | PendVal v -> walk_v v
         | _ -> ());
        List.iter pend_waiters walk_token)
  and walk_v = function
    | VClosure(_,_,env) -> walk_env env
    | _ -> ()
  and walk_env env =
    on_new envs env (fun () ->
        Array.iter env (function
            | Value v -> walk_v v
            | Pending p -> walk_pending p))
  and walk_frame frame =
    on_new frames frame (fun () ->
        match frame with
        | FPruning { pending } -> walk_pending pending
        | FCall(env) -> walk_env env
        | _ -> ())
  and walk_stack = List.iter ~f: walk_frame in
  List.iter blocks (fun (i, token) -> walk_token token);
  let blocks' = List.concat_map blocks serialize_block in
  M.List [(M.Int current_coeffect);
          M.List (List.concat_map !frames serialize_frame);
          M.List (List.concat_map !pendings serialize_pending);
          M.List (List.concat_map !envs serialize_env);
          M.List blocks']
  |> Msgpck.String.to_string

let deserialize s =
  let (_, packed) = M.String.read s in
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
  let dummy_env size = Array.create size (Value (VConst (Ast.Null))) in
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
        add_repo frames_repo id @@ FPruning { instances; pending = repo_or_dummy_pending pending};
        deserialize_frames xs
      | (M.Int id)::(M.Int 1)::(M.Bool first_value)::(M.Int instances)::(M.Int pc)::(M.Int c)::xs ->
        add_repo frames_repo id @@ FOtherwise { first_value; instances; pc = (pc, c) };
        deserialize_frames xs
      | (M.Int id)::(M.Int 2)::(M.Int i)::(M.Int pc)::(M.Int c)::xs ->
        let i' = if Int.equal (-1) i then None else Some(i) in
        add_repo frames_repo id @@ FSequential(i', (pc, c));
        deserialize_frames xs
      | (M.Int id)::(M.Int 3)::(M.Int env)::xs ->
        add_repo frames_repo id @@ FCall(repo_or_dummy_env env);
        deserialize_frames xs
      | (M.Int id)::(M.Int 4)::xs ->
        add_repo frames_repo id FResult;
        deserialize_frames xs
      | _ -> raise BadFormat in
    let deserialize_token = function
      | M.List [M.Int pc; M.Int c; M.List frames; M.Int env] ->
        let stack = List.map frames (function
            | M.Int id -> repo_find frames_repo id |> Option.value_exn
            | _ -> raise BadFormat) in
        { pc = (pc, c); stack; env = repo_or_dummy_env env}
      | _ -> raise BadFormat in
    let deserialize_tokens = List.map ~f:deserialize_token in
    let deserialize_value = function
      | (M.Int 0)::v::xs -> (VConst(deserialize_const v), xs)
      | (M.Int 1)::(M.Int pc)::(M.Int to_copy)::(M.Int env)::xs ->
        (VClosure(pc, to_copy, repo_or_dummy_env env), xs)
      | _ -> raise BadFormat in
    let rec deserialize_pendings = function
      | [] -> ()
      | (M.Int id)::xs ->
        let (v, tokens, xs') = match xs with
          | (M.Int 0)::(M.List tokens)::xs' ->
            (Pend, deserialize_tokens tokens, xs')
          | (M.Int 1)::xs' ->
            let (v, xs'') = deserialize_value xs' in
            (PendVal v, [], xs'')
          | (M.Int 2)::xs' -> (PendStopped, [], xs)
          | _ -> raise BadFormat in
        let p = repo_or_dummy_pending id in
        p.pend_value <- v;
        p.pend_waiters <- tokens;
        deserialize_pendings xs'
      | _ -> raise BadFormat in
    let deserialize_env_value = function
      | M.List ((M.Int 0)::xs) ->
        let (v, _) = deserialize_value xs in
        Value v
      | M.List [M.Int 1; M.Int pending] ->
        Pending (repo_or_dummy_pending pending)
      | _ -> raise BadFormat in
    let rec deserialize_envs = function
      | [] -> ()
      | (M.Int id)::(M.List vals)::xs ->
        let e = repo_or_dummy_env id in
        List.iteri vals (fun i v ->
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
