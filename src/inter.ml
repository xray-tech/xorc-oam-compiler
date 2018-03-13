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
    | VPending { pend_value; pend_waiters } ->
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
    | Error of { thread : int; ffi : string; args : v list}

  type trace = action list
end

module Res = struct
  type t = { values : v list;
             coeffects : coeffect list;
             killed : int list;
             instance : instance}
end

exception TODO
exception RuntimeError

let rec increment_instances = function
  | [] -> ()
  | FOtherwise(r)::_ -> r.instances <- r.instances + 1
  | FPruning(r)::_ -> r.instances <- r.instances + 1
  | _::xs -> increment_instances xs

let alloc_env len = Array.create ~len (Var.dummy, (VConst Ast.Null))

let print_debug { code } (pc, c) env  =
  let (size, vars, f) = code.(pc) in
  let op = f.(c) in
  Stdio.printf "Op (%i:%i:size %i): %s Env: %s\n"
    pc c size
    (sexp_of_t op |> Sexp.to_string_hum)
    (format_env env)

let get_code inter pc =
  inter.code.(pc)

let call_bindings {id} args = []

let rec is_alive = function
  | [] -> true
  | FPruning { pending = { pend_value = PendStopped } }::_
  | FPruning { pending = { pend_value = PendVal(_)} }::_ ->
    false
  | FPruning { pending = { pend_value = Pend; pend_waiters } }::_ when waiters_are_dead pend_waiters ->
    false
  | _::xs -> is_alive xs
and waiters_are_dead = function
  | [] -> true
  | { stack }::xs -> if is_alive stack then false else waiters_are_dead xs

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
  Array.iteri args ~f:(fun i arg ->
      let (_, v) = from.(arg) in
      to_.(offset + i) <- (List.nth_exn vars i, v))

let rec publish state ({id; stack; env} as thread) v =
  match stack with
  | [] -> assert false
  | x::stack' -> match x with
    | FResult ->
      state.values <- v::state.values;
      ([], [D.PublishedValue v;
            D.HaltedThread id])
    | FSequential(var, op) ->
      (match var with
       | None -> ()
       | Some var ->
         env.(Var.index var) <- (var, v));
      ([{thread with op; stack = stack'}],
       [])
    | FOtherwise r ->
      r.instances <- r.instances - 1;
      r.first_value <- true;
      publish state { thread with stack = stack' } v
    | FPruning ({ pending = ({ pend_value = Pend } as p) } as r) ->
      r.instances <- r.instances - 1;
      concat2 [pending_realize state p v;
               ([], [D.HaltedThread id])]
    | FPruning ({ pending = { pend_value = PendVal(_) } } as r) ->
      r.instances <- r.instances - 1;
      ([], [D.HaltedThread id])
    | FPruning { pending = { pend_value = PendStopped } } -> assert false
    | FCall env' ->
      publish state { thread with stack = stack'; env = env' } v
and halt state ({ id; stack; env} as thread) =
  let rec in_stack = function
    | [] -> ([], [])
    | x::stack' -> match x with
      | FOtherwise {instances = 1 ;
                    first_value = false; op } ->
        tick state { thread with op; stack = stack' }
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
and pending_realize state p v =
  p.pend_value <- PendVal v;
  concat2_map p.pend_waiters ~f:(fun ({ id; stack } as thread) ->
      if is_alive stack
      then tick state thread
      else ([], []))
and pending_stop state p =
  p.pend_value <- PendStopped;
  concat2_map p.pend_waiters ~f:(fun thread ->
      tick state thread)
and tick
    ({ instance; inter } as state)
    ({ op = (pc, c); stack; env} as thread) =
  (* print_debug inter (pc, c) env; *)
  let realized arg =
    (match env.(arg) with
     | (_, VPending ({ pend_value = Pend } as p)) ->
       `Pending p
     | (_, VPending { pend_value = PendStopped }) ->
       `Stopped
     | (_, VPending { pend_value = PendVal(v) })
     | (_, v) -> `Value v) in
  let realized_multi args =
    let args' = Array.map args ~f:realized in
    if Array.find args' ~f:(function
        | `Stopped -> true
        | _ -> false) |> Option.is_some
    then `Stopped
    else
      let values = (Array.create ~len:(Array.length args) (VConst (Ast.Null))) in
      let rec step = function
        | i when Int.equal (Array.length args) i -> `Values values
        | i ->
          match args'.(i) with
          | `Pending p -> `Pending p
          | `Stopped -> `Stopped
          | `Value v ->
            values.(i) <- v;
            step (i + 1) in
      step 0 in
  let call_ffi index args =
    match realized_multi args with
    | `Pending p ->
      p.pend_waiters <- thread::p.pend_waiters;
      ([], [])
    | `Stopped -> halt state thread
    | `Values args' ->
      let impl = Env.get_ffi inter.env_snapshot index in
      (* Stdio.eprintf "---CALL %i\n" prim;
       * Array.iter args' (fun v -> Stdio.eprintf "--ARG: %s\n" (sexp_of_v v |> Sexp.to_string_hum)); *)
      (match impl args' with
       | PrimVal res ->
         publish state thread res
       | PrimHalt -> halt state thread
       | PrimUnsupported ->
         (* TODO warning? *)
         let ffi = Env.get_ffi_name inter.env_snapshot index in
         concat2 [halt state thread;
                  ([], [D.Error { thread = thread.id;
                                  ffi;
                                  args = Array.to_list args'}])]
       | PrimPendingSubscribe p ->
         p.pend_waiters <- thread::p.pend_waiters;
         ([], [])
       | PrimPendingRealize (p, v) ->
         concat2 [pending_realize state p v;
                  publish state thread (VConst Ast.Signal)]
       | PrimPendingStop p ->
         concat2 [pending_stop state p;
                  publish state thread (VConst Ast.Signal)]) in
  let (_, _, proc) = get_code inter pc in
  let (op, pos) = proc.(c) in
  match op with
  | Const v ->
    publish state thread (VConst v)
  | Label i ->
    publish state thread (VLabel i)
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
     | Some var -> env.(Var.index var) <- (var, VPending pending));
    let new_id = new_thread_id state in
    ([{id = new_id; op = (pc, c2); stack = (frame::stack); env; pos = thread.pos};
      {thread with op = (pc, c1) }],
     [NewThread new_id])
  | Sequential(c1, var, c2) ->
    let frame = FSequential(var, (pc, c2)) in
    ([{ thread with op = (pc, c1); stack = (frame::stack)}],
     [])
  | Closure (pc', to_copy) ->
    publish state thread (VClosure(pc', to_copy, env))
  | Call(TFun(pc'), args) ->
    let (size, vars, f_code) = get_code inter pc' in
    let env' = alloc_env size in
    let frame = FCall(env) in
    copy_env ~offset: 0 ~from:env ~to_:env' ~vars ~args;
    ([{thread with op = (pc', Array.length f_code - 1);
                   stack = (frame::stack);
                   env = env'}],
     call_bindings thread args)
  | TailCall(TFun(pc'), args) ->
    let (_, vars, f_code) = get_code inter pc' in
    copy_env ~offset: 0 ~from:env ~to_:env ~vars ~args;
    ([{thread with op = (pc', Array.length f_code - 1)}],
     call_bindings thread args)
  | Call(TDynamic(i), args) ->
    (match realized i with
     | `Pending p ->
       p.pend_waiters <- thread::p.pend_waiters;
       ([], [])
     | `Stopped -> halt state thread
     | `Value(VClosure(pc', to_copy, closure_env)) ->
       let (size, vars, f_code) = get_code inter pc' in
       let env' = alloc_env size in
       for i = 0 to to_copy - 1 do
         env'.(i) <- closure_env.(i)
       done;
       copy_env ~offset:to_copy ~from:env ~to_:env' ~vars ~args;
       let frame = FCall(env) in
       ([{ thread with op = (pc', Array.length f_code - 1);
                       stack = (frame::stack);
                       env = env'}],
        call_bindings thread args)
     | `Value(VLabel(pc')) ->
       let (size, vars, f_code) = get_code inter pc' in
       let env' = alloc_env size in
       copy_env ~offset: 0 ~from:env ~to_:env' ~vars ~args;
       let frame = FCall(env) in
       ([{ thread with op = (pc', Array.length f_code - 1);
                       stack = (frame::stack);
                       env = env'}],
        call_bindings thread args)
     | `Value(VTuple(vs)) ->
       (match realized args.(0) with
        | `Pending p ->
          p.pend_waiters <- thread::p.pend_waiters;
          ([], [])
        | `Stopped -> raise Util.TODO
        | `Value(VConst(Ast.Int i)) ->
          publish state thread (List.nth_exn vs i)
        | `Value(_) -> raise Util.TODO)
     | `Value(_) -> raise Util.TODO)
  | Coeffect arg ->
    (match realized arg with
     | `Pending p ->
       p.pend_waiters <- thread::p.pend_waiters;
       ([], [])
     | `Stopped -> halt state thread
     | `Value(descr) ->
       let thread' = { thread with env = Array.copy(env)} in
       let coeff_id = instance.current_coeffect in
       state.coeffects <- (coeff_id, descr)::state.coeffects;
       instance.blocks <- (coeff_id, thread')::instance.blocks;
       instance.current_coeffect <- instance.current_coeffect + 1;
       ([], [D.Coeffect { thread = thread.id; id = coeff_id; desc = descr}]))
  | FFI(target, args) ->
    call_ffi target args
  | TailCall(TDynamic(_), _) -> raise Util.TODO

let debug_tick state thread =
  let update_pos ({ op = (pc, c) } as th) =
    let (_, _, proc) = get_code state.inter pc in
    let (_, pos) = proc.(c) in
    { th with pos } in
  let (threads, trace) = tick state thread in
  (List.map threads ~f:update_pos, trace)

let check_killed ?(ignore_fun=(fun _ -> false)) instance =
  let f (killed, acc) ((id, {stack}) as block) =
    if ignore_fun id
    then (killed, acc)
    else if is_alive stack
    then (killed, block::acc)
    else (id::killed, acc) in
  let (killed, blocks) = List.fold instance.blocks ~init:([], []) ~f in
  (killed, {instance with blocks})

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
      let (threads', _) = tick state thread in
      step (threads' @ threads) in
  step threads

let run inter =
  let (state, threads) = init inter in
  run_loop state threads;
  let (killed, instance') = check_killed state.instance in
  Res.{ values = state.values;
        coeffects = state.coeffects;
        killed;
        instance = instance' }

let inter {ffi; code} =
  let open Result.Let_syntax in
  let%map env_snapshot = Env.snapshot ffi in
  { code; env_snapshot }


let unblock inter instance coeffect value =
  let state = { thread_id = 0;
                inter; instance;
                values = [];
                coeffects = [];} in
  match List.Assoc.find instance.blocks ~equal:Int.equal coeffect with
  | None -> Res.{ values = [];
                  coeffects = [];
                  killed = [];
                  instance}
  | Some(thread) ->
    let (threads, _) = publish state thread value in
    run_loop state threads;
    let (killed, instance') = check_killed ~ignore_fun:(fun id -> Int.equal id coeffect) instance in
    Res.{ values = state.values;
          coeffects = state.coeffects;
          killed;
          instance = instance'}

let is_running { blocks } =
  not (List.is_empty blocks)
