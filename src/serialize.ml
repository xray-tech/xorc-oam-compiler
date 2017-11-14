open Base

module M = Msgpck

let serialize_const = function
  | Ast.Int x -> M.Int x
  | Float x -> M.Float x
  | String x -> M.String x
  | Signal -> M.Ext(1, "")
  | Null -> M.Nil
  | Bool x -> M.Bool x

let serialize_bc code =
  let array_to_list_map a ~f =
    Array.to_sequence a |> Sequence.map ~f |> Sequence.to_list in
  let obj = array_to_list_map code (fun (i, ecode) ->
      M.List ((M.Int i)::(List.concat (array_to_list_map ecode (function
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
          | Inter.Coeffect(v) -> [M.Int 6; M.String v]
          | Inter.Stop -> [M.Int 7]
          | Inter.Const(v) -> [M.Int 8; serialize_const v]
          | Inter.Closure(p, to_copy) -> [M.Int 9; M.Int p; M.Int to_copy]
        ))))) in
  Msgpck.String.to_string (M.List obj)

exception BadFormat

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
    | ((M.Int i)::(M.Int t)::(M.Int t_arg)::(M.List args)::xs) ->
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
    | ((M.Int 6)::(M.String s)::xs) ->
      Inter.Coeffect(s)::(des_fun xs)
    | ((M.Int 7)::xs) ->
      Inter.Stop::(des_fun xs)
    | ((M.Int 8)::v::xs) ->
      let v' = match v with
        | M.Int x -> Ast.Int x
        | M.Float x -> Ast.Float x
        | M.String x -> Ast.String x
        | M.Ext(1,_) -> Ast.Signal
        | M.Nil -> Ast.Null
        | M.Bool x -> Ast.Bool x
        | _ -> raise BadFormat in
      Inter.Const(v')::(des_fun xs)
    | ((M.Int 9)::(M.Int pc)::(M.Int to_copy)::xs) ->
      Inter.Closure(pc, to_copy)::(des_fun xs)
    | _ -> raise BadFormat in
  match packed with
  | M.List xs -> Array.of_list (List.map xs (function
      | M.List ((M.Int i)::xs) -> (i, Array.of_list @@ des_fun xs)
      | _ -> raise BadFormat))
  | _ -> raise BadFormat

open Inter
let serialize { current_coeffect; blocks } =
  let id = ref 0 in
  let make_id () = id := !id + 1; !id in
  let frames = ref [] in
  let envs = ref [] in
  let pendings = ref [] in
  let dedup cache obj =
    match List.find_map !cache ~f:(function
        | (id, obj') when phys_equal obj obj' -> Some(id)
        | _ -> None) with
    | Some id -> M.Int id
    | None ->
      let id = make_id () in
      cache := (id, obj)::!cache;
      M.Int id in
  let serialize_frame (id, frame) =
    let f = match frame with
      | FPruning { realized; instances; pending } ->
        [M.Int 0; M.Bool realized; M.Int instances; dedup pendings pending]
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
    let v = match pend_value with
      | Pend -> [M.Int 0]
      | PendVal v -> (M.Int 1)::(serialize_value v)
      | PendStopped -> [M.Int 2] in
    let tokens = List.map pend_waiters serialize_token in
    (M.Int id)::(v @ tokens)
  and serialize_value = function
    | VConst x ->
      [M.Int 0; serialize_const x]
    | VClosure(pc, to_copy, env) ->
      [M.Int 0; M.Int to_copy; dedup envs env]
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
  let blocks = List.concat_map blocks serialize_block in
  M.List [(M.Int current_coeffect);
          M.List (List.concat_map !frames serialize_frame);
          M.List (List.concat_map !pendings serialize_pending);
          M.List (List.concat_map !envs serialize_env);
          M.List blocks;]

let deserialize s =
  let (_, packed) = M.String.read s in
  let frames = ref [] in
  let envs = ref [] in
  let pendings = ref [] in
  let cache_find cache id =
    List.Assoc.find_exn !cache ~equal:Int.equal id in
  let rec deserialize_frames = function
  | [] -> []
  | [M.Int id; M.Int 0; M.Bool realized; M.Int instances; M.Int pending]::xs ->
    FPruning { realized; instances; pending = cache_find pendings pending}::
    (deserialize_frames xs)
  | [M.Int 1; M.Bool first_value; M.Int instances; M.Int pc; M.Int c] ->
  | [M.Int 2; M.Int i; M.Int pc; M.Int c] ->
  | [M.Int 3; M.Int env]in


  match packed with
  | M.List [M.Int current_coeffect;
            M.List frames;
            M.List pendings;
            M.List envs;
            M.List blocks] ->
    deserialize_frames frames;
    deserialize_pendings pendings;
    deserialize_envs envs;
    let blocks = deserialize_blocks blocks in
    { current_coeffect; blocks = blocks' }
  | _ -> raise BadFormat
