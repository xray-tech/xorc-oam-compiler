open Base
include Inter0

module Value = struct
  include (val Comparator.make ~compare:compare_v ~sexp_of_t:sexp_of_v)

  let const_to_string = function
    | Ast.Int x -> Int.to_string x
    | Float x -> Float.to_string x
    | String x -> Printf.sprintf "\"%s\"" x
    | Signal -> "signal"
    | Null -> "null"
    | Bool true -> "true"
    | Bool false -> "false"

  let rec to_string = function
    | VConst x -> const_to_string x
    | VClosure (fun_, _, _) -> Printf.sprintf "<closure #%d>" fun_
    | VLabel fun_ -> Printf.sprintf "<label #%d>" fun_
    | VTuple xs ->
      Printf.sprintf "(%s)" (String.concat ~sep:", " (List.map xs ~f:to_string))
    | VList xs ->
      Printf.sprintf "[%s]" (String.concat ~sep:", " (List.map xs ~f:to_string))
    | VRecord pairs ->
      let format_pair (k, v) = Printf.sprintf "%s = %s" k (to_string v) in
      let pairs' = List.map pairs ~f:format_pair in
      Printf.sprintf "{. %s .}" (String.concat ~sep:", " pairs')
    | VRef x -> Printf.sprintf "<ref %s>" (to_string !x)
    | VPending x -> pending_to_string x
  and pending_to_string { pend_value; pend_waiters } =
    let format_value = function
      | PendVal x -> to_string x
      | PendStopped -> "<stopped>"
      | Pend -> "<waiting>" in
    Printf.sprintf "<pending value: %s waiters: %d>"
      (format_value pend_value) (List.length pend_waiters)

end

type instance = { mutable current_coeffect : int;
                  mutable blocks : (int * thread) list }

type inter = {
  code : code;
  env_snapshot : Env.snapshot;
}

type coeffect = int * v

type state = {
  mutable thread_id : int;
  mutable values : v list;
  mutable coeffects : coeffect list;
  inter : inter;
  instance : instance;
}

module D = struct
  type threads = thread list
  type action =
    | PublishedValue of v
    | NewThread of int
    | HaltedThread of int
    | Coeffect of { thread: int; id: int; desc: v }
    | Error of { thread : int; ffc : string; args : v list}

  type trace = action list
end

module Res = struct
  type t = { values : v list;
             coeffects : coeffect list;
             instance : instance}
end

exception TODO

let rec increment_instances = function
  | [] -> ()
  | FOtherwise(r)::_ -> r.instances <- r.instances + 1
  | FPruning(r)::_ -> r.instances <- r.instances + 1
  | _::xs -> increment_instances xs

let alloc_env len = Array.create ~len (Var.dummy, (Value (VConst Ast.Null)))

let print_debug { code } (pc, c) env  =
  let (size, _, f) = code.(pc) in
  let op = f.(c) in
  Stdio.printf "Op (%i:%i:size %i): %s Env: %s\n"
    pc c size
    (sexp_of_t op |> Sexp.to_string_hum)
    (format_env env)

let get_code inter pc =
  inter.code.(pc)

(* TODO *)
let call_bindings _ _ = []

let concat2_map l ~f =
  List.fold l ~init:([], []) ~f:(fun (part1, part2) v ->
      let (part1', part2') = f v in
      (part1' @ part1, part2' @ part2))

let concat2 l =
  List.fold l ~init:([], []) ~f:(fun (part1, part2) (part1', part2') ->
      (part1' @ part1, part2' @ part2))

let new_thread_id state =
  let id = state.thread_id in
  state.thread_id <- id + 1;
  id

let copy_env ~vars ~offset ~to_ ~from ~args =
  let to' = if Array.length to_ < offset + Array.length args
    then alloc_env (offset + Array.length args)
    else to_ in
  Array.iteri args ~f:(fun i arg ->
      let (_, v) = from.(arg) in
      let var = match List.nth vars i with
        | Some var -> var
        | None -> Var.dummy in
      to'.(offset + i) <- (var, v));
  to'

let rec publish state ({stack; env} as thread) v =
  match stack with
  | [] -> assert false
  | x::stack' -> match x with
    | FResult ->
      state.values <- v::state.values;
      ([], [D.PublishedValue v])
    | FSequential(var, op) ->
      increment_instances stack;
      (match var with
       | None -> ()
       | Some var ->
         env.(Var.index var) <- (var, Value v));
      ([{thread with op; stack = stack'}],
       [])
    | FOtherwise r ->
      r.first_value <- true;
      publish state { thread with stack = stack' } v
    | FPruning { pending = ({ pend_value = Pend } as p) } ->
      pending_realize state p v
    | FPruning { pending = { pend_value = PendVal(_) | PendStopped } } ->
      ([], [])
    | FCall env' ->
      publish state { thread with stack = stack'; env = env' } v
and halt state ({ id; stack } as thread) =
  let rec in_stack = function
    | [] -> ([], [])
    | x::stack' -> match x with
      | FOtherwise {instances = 1 ;
                    first_value = false; op } ->
        tick state { thread with op; stack = stack' } true
      | FOtherwise { instances = 1} ->
        in_stack stack'
      | FOtherwise r ->
        r.instances <- r.instances - 1;
        ([], [])
      | FPruning { instances = 1; pending = ({ pend_value = Pend } as p)} ->
        pending_stop state p
      | FPruning r ->
        r.instances <- r.instances - 1;
        ([], [])
      | FCall env' ->
        halt state { thread with stack = stack'; env = env' }
      | _ -> in_stack stack' in
  concat2 [in_stack stack;
           ([], [D.HaltedThread id])]
and publish_and_halt state thread v =
  (* execution order is important here! *)
  let r1 = publish state thread v in
  let r2 = halt state thread in
  concat2 [r1; r2]
and pending_realize state p v =
  p.pend_value <- PendVal v;
  concat2_map p.pend_waiters ~f:(fun thread ->
      tick state thread false)
and pending_stop state p =
  p.pend_value <- PendStopped;
  concat2_map p.pend_waiters ~f:(fun thread ->
      tick state thread false)
and tick
    ({ instance; inter } as state)
    ({ op = (pc, c); stack; env} as thread)
    subscribe =
  (* print_debug inter (pc, c) env; *)
  let realized arg =
    (match env.(arg) with
     | (_, Pending ({ pend_value = Pend } as p)) ->
       `Pending p
     | (_, Pending { pend_value = PendStopped }) ->
       `Stopped
     | (_, Pending { pend_value = PendVal(v) })
     | (_, Value v) -> `Value v) in
  let realized_multi args =
    let args' = Array.map args ~f:realized in
    let (values, pendings, stopped) = Array.fold args' ~init:([], [], false)
        ~f:(fun (values, pendings, stopped) v ->
            match v with
            | `Pending p -> (values, p::pendings, stopped)
            | `Value v -> (v::values, pendings, stopped)
            | `Stopped -> (values, pendings, true)) in
    if stopped then `Stopped
    else if List.length pendings > 0 then `Pendings pendings
    else `Values (Array.of_list_rev values) in
  let call_tuple vs args =
    (* TODO log errors *)
    (match realized args.(0) with
     | `Pending p ->
       if subscribe then p.pend_waiters <- thread::p.pend_waiters;
       ([], [])
     | `Stopped -> raise Util.TODO
     | `Value(VConst(Ast.Int i)) ->
       (match List.nth vs i with
        | Some v -> publish_and_halt state thread v
        | None -> halt state thread)
     | `Value(_) -> halt state thread) in
  let call_ffc index args =
    match realized_multi args with
    | `Pendings ps ->
      if subscribe then
        List.iter ps ~f:(fun p -> p.pend_waiters <- thread::p.pend_waiters);
      ([], [])
    | `Stopped -> halt state thread
    | `Values args' ->
      let name = Env.get_ffc_name inter.env_snapshot index in
      let unsupported () =
        concat2 [halt state thread;
                 ([], [D.Error { thread = thread.id;
                                 ffc = name;
                                 args = Array.to_list args'}])] in
      match name with
      | "core.make-pending" ->
        (match args' with
         | [||] -> publish_and_halt state thread (VPending { pend_value = Pend; pend_waiters = [] })
         | _ -> unsupported ())
      | "core.pending-read" ->
        (match args' with
         | [| VPending { pend_value = PendVal v } |] ->
           publish_and_halt state thread v
         | [| VPending { pend_value = PendStopped } |] ->
           halt state thread
         | [| VPending p |] ->
           if subscribe then p.pend_waiters <- thread::p.pend_waiters;
           ([], [])
         | _ -> unsupported ())
      | "core.realize" ->
        (match args' with
         | [| VPending ({ pend_value = Pend } as p); v|] ->
           concat2 [pending_realize state p v;
                    publish_and_halt state thread (VConst Ast.Signal)]
         | [| VPending _; _ |] ->
           publish_and_halt state thread (VConst Ast.Signal)
         | _ -> unsupported ())
      | "core.is-realized" ->
        (match args' with
         | [| VPending { pend_value = PendVal _ } |] ->
           publish_and_halt state thread (VConst (Ast.Bool true))
         | [| VPending _ |] ->
           publish_and_halt state thread (VConst (Ast.Bool false))
         | _ -> unsupported ())
      | "core.stop-pending" ->
        (match args' with
         | [| VPending ({ pend_value = Pend } as p) |] ->
           concat2 [pending_stop state p;
                    publish_and_halt state thread (VConst Ast.Signal)]
         | [| VPending _ |] ->
           publish_and_halt state thread (VConst Ast.Signal)
         | _ -> unsupported ())
      | _ ->
        let impl = Env.get_ffc inter.env_snapshot index in
        (* Stdio.eprintf "---CALL %i\n" prim;
         * Array.iter args' (fun v -> Stdio.eprintf "--ARG: %s\n" (sexp_of_v v |> Sexp.to_string_hum)); *)
        try (match impl args' with
            | PrimVal res -> publish_and_halt state thread res
            | PrimHalt -> halt state thread
            | PrimUnsupported -> unsupported ())
        with _ -> unsupported () in
  let (_, _, proc) = get_code inter pc in
  let (op, _) = proc.(c) in
  match op with
  | Const v ->
    publish_and_halt state thread (VConst v)
  | Label i ->
    publish_and_halt state thread (VLabel i)
  | Stop -> halt state thread
  | Parallel(c1, c2) ->
    increment_instances stack;
    let new_id = new_thread_id state in
    ([{thread with op = (pc, c1)};
      { id = new_id; env = (Array.copy env); op = (pc, c2); stack; pos = thread.pos }],
     [NewThread new_id])
  | Otherwise(c1, c2) ->
    let frame = FOtherwise { first_value = false;
                             instances = 1;
                             op = (pc, c2); } in
    ([{thread with op = (pc, c1); stack = (frame::stack)}],
     [])
  | Pruning(c1, var, c2) ->
    let pending = { pend_value = Pend; pend_waiters = [] } in
    let frame = FPruning { instances = 1;
                           pending;} in
    (match var with
     | None -> ()
     | Some var -> env.(Var.index var) <- (var, Pending pending));
    let new_id = new_thread_id state in
    ([{id = new_id; op = (pc, c2); stack = (frame::stack); env; pos = thread.pos};
      {thread with op = (pc, c1) }],
     [NewThread new_id])
  | Sequential(c1, var, c2) ->
    let frame = FSequential(var, (pc, c2)) in
    ([{ thread with op = (pc, c1); stack = (frame::stack)}],
     [])
  | Closure (pc', to_copy) ->
    publish_and_halt state thread (VClosure(pc', to_copy, env))
  | Call(TFun(pc'), args) ->
    let (size, vars, f_code) = get_code inter pc' in
    let env' = alloc_env size in
    let frame = FCall(env) in
    let env'' = copy_env ~offset: 0 ~from:env ~to_:env' ~vars ~args in
    ([{thread with op = (pc', Array.length f_code - 1);
                   stack = (frame::stack);
                   env = env''}],
     call_bindings thread args)
  | TailCall(TFun(pc'), args) ->
    let (size, vars, f_code) = get_code inter pc' in
    let env' = if size > Array.length env
      then alloc_env size
      else env in
    let env'' = copy_env ~offset: 0 ~from:env ~to_:env' ~vars ~args in
    ([{thread with op = (pc', Array.length f_code - 1);
                   env = env''}],
     call_bindings thread args)
  | Call(TDynamic(i), args) ->
    (match realized i with
     | `Pending p ->
       if subscribe then p.pend_waiters <- thread::p.pend_waiters;
       ([], [])
     | `Stopped -> halt state thread
     | `Value(VClosure(pc', to_copy, closure_env)) ->
       let (size, vars, f_code) = get_code inter pc' in
       let env' = alloc_env size in
       let env'' = copy_env ~offset:to_copy ~from:env ~to_:env' ~vars ~args in
       for i = 0 to to_copy - 1 do
         env''.(i) <- closure_env.(i)
       done;
       let frame = FCall(env) in
       ([{ thread with op = (pc', Array.length f_code - 1);
                       stack = (frame::stack);
                       env = env''}],
        call_bindings thread args)
     | `Value(VLabel(pc')) ->
       let (size, vars, f_code) = get_code inter pc' in
       let env' = alloc_env size in
       let env'' = copy_env ~offset: 0 ~from:env ~to_:env' ~vars ~args in
       let frame = FCall(env) in
       ([{ thread with op = (pc', Array.length f_code - 1);
                       stack = (frame::stack);
                       env = env''}],
        call_bindings thread args)
     | `Value(VTuple(vs)) -> call_tuple vs args
     | `Value(_) ->
       (* TODO log error *)
       halt state thread)
  | Coeffect arg ->
    (match realized arg with
     | `Pending p ->
       if subscribe then p.pend_waiters <- thread::p.pend_waiters;
       ([], [])
     | `Stopped -> halt state thread
     | `Value(descr) ->
       let thread' = { thread with env = Array.copy(env)} in
       let coeff_id = instance.current_coeffect in
       state.coeffects <- (coeff_id, descr)::state.coeffects;
       instance.blocks <- (coeff_id, thread')::instance.blocks;
       instance.current_coeffect <- instance.current_coeffect + 1;
       ([], [D.Coeffect { thread = thread.id; id = coeff_id; desc = descr}]))
  | FFC(target, args) ->
    call_ffc target args
  | TailCall(TDynamic(i), args) ->
    (match realized i with
     | `Pending p ->
       if subscribe then p.pend_waiters <- thread::p.pend_waiters;
       ([], [])
     | `Stopped -> halt state thread
     | `Value(VClosure(pc', to_copy, closure_env)) ->
       let (size, vars, f_code) = get_code inter pc' in
       let env' = if size > Array.length env
         then alloc_env size
         else env in
       let env'' = copy_env ~offset:to_copy ~from:env ~to_:env' ~vars ~args in
       for i = 0 to to_copy - 1 do
         env''.(i) <- closure_env.(i)
       done;
       ([{ thread with op = (pc', Array.length f_code - 1);
                       env = env''}],
        call_bindings thread args)
     | `Value(VLabel(pc')) ->
       let (size, vars, f_code) = get_code inter pc' in
       let env' = if size > Array.length env
         then alloc_env size
         else env in
       let env'' = copy_env ~offset: 0 ~from:env ~to_:env' ~vars ~args in
       ([{ thread with op = (pc', Array.length f_code - 1);
                       env = env''}],
        call_bindings thread args)
     | `Value(VTuple(vs)) -> call_tuple vs args
     | `Value(_) ->
       (* TODO log error *)
       halt state thread)

let update_pos state ({ op = (pc, c) } as th) =
  let (_, _, proc) = get_code state.inter pc in
  let (_, pos) = proc.(c) in
  { th with pos }

let debug_tick state thread =
  let (threads, trace) = tick state thread true in
  (List.map threads ~f:(update_pos state), trace)

let debug_unblock state coeffect value =
  match List.Assoc.find state.instance.blocks ~equal:Int.equal coeffect with
  | None -> None
  | Some(thread) ->
    let (threads, trace) = publish_and_halt state thread value in
    Some (List.map threads ~f:(update_pos state), trace)

let init inter =
  let instance = { current_coeffect = 0;
                   blocks = [] } in
  let (init_env_size, _, e_code) = inter.code.(0) in
  let state = {thread_id = 0;
               inter; instance;
               values = [];
               coeffects = []; } in
  (state,
   [{id = new_thread_id state;
     op = (0, Array.length e_code - 1);
     env = alloc_env init_env_size;
     stack = [FResult];
     pos = Ast.Pos.{ line = 0; col = 0; path = ""}}])

let run_loop state threads =
  let rec step = function
    | [] -> ()
    | thread::threads ->
      let (threads', _) = tick state thread true in
      step (threads' @ threads) in
  step threads

let run inter =
  let (state, threads) = init inter in
  run_loop state threads;
  Res.{ values = state.values;
        coeffects = state.coeffects;
        instance = state.instance}

let inter {ffc; code} =
  let open Result.Let_syntax in
  let%map env_snapshot = Env.snapshot ffc in
  { code; env_snapshot }

let unblock inter { current_coeffect; blocks } coeffect value =
  (* we don't want to mutate original instance *)
  let instance' = { current_coeffect;
                    blocks = List.Assoc.remove blocks ~equal:Int.equal coeffect } in
  let state = { thread_id = 0;
                inter; instance = instance';
                values = [];
                coeffects = [];} in
  match List.Assoc.find blocks ~equal:Int.equal coeffect with
  | None -> Res.{ values = [];
                  coeffects = [];
                  instance = instance'}
  | Some(thread) ->
    let (threads, _) = publish_and_halt state thread value in
    run_loop state threads;
    Res.{ values = state.values;
          coeffects = state.coeffects;
          instance = state.instance}

let is_running { blocks } =
  not (List.is_empty blocks)
