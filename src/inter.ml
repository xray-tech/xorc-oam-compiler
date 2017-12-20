open Base
include Inter0

module Value = struct
  include (val Comparator.make ~compare:compare_v ~sexp_of_t:sexp_of_v)
end

type bc = (int * t array) array [@@deriving sexp, compare]

type instance = { mutable current_coeffect : int;
                  mutable blocks : (int * token) list }

type state = { code : bc;
               deps : bc;
               instance : instance;
               mutable queue : token list;
               prims : prims;
               values: (v -> unit);
               coeffects: ((int * v) -> unit)}

type coeffect = int * v

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

let alloc_env len = Array.create ~len (Value (VConst Ast.Null))

(* let rec format_value = function
 *   | Value v -> format_v v
 *   | Pending { pend_value = PendVal v} -> Printf.sprintf "(Pending %s)" (format_v v)
 *   | Pending { pend_value = v} -> sexp_of_pend_value v |> Sexp.to_string_hum
 * and format_v = function
 *   | VConst v  -> Ast.sexp_of_const v |> Sexp.to_string_hum
 *   | VClosure (i, _, _) -> Printf.sprintf "(Closure %i)" i
 *   | VTu
 *
 * let format_env env =
 *   let vs = (Array.to_sequence env
 *             |> Sequence.map ~f:format_value
 *             |> Sequence.to_list) in
 *   "(" ^ String.concat ~sep:", " vs ^ ")"
 *
 * let print_debug { code } (pc, c) env  =
 *   let (size, f) = code.(pc) in
 *   let op = f.(c) in
 *   Stdio.printf "Op (%i:%i:size %i): %s Env: %s\n"
 *     pc c size
 *     (sexp_of_t op |> Sexp.to_string_hum)
 *     (format_env env) *)

let get_code state pc =
  if pc < 0
  then state.code.(Int.abs pc - 1)
  else state.deps.(pc)

let rec publish state stack env v =
  match stack with
  | [] -> assert false
  | x::stack' -> match x with
    | FResult -> state.values v
    | FSequential(i, pc) ->
      increment_instances stack';
      (match i with
       | None -> ()
       | Some i ->
         env.(i) <- Value(v));
      tick state pc stack' env
    | FOtherwise ({ first_value = false } as r) ->
      r.first_value <- true;
      publish state stack' env v
    | FOtherwise _ ->
      publish state stack' env v
    | FPruning ({ pending = { pend_value = Pend } } as r) ->
      r.pending.pend_value <- PendVal v;
      List.iter r.pending.pend_waiters ~f:(fun { pc; stack; env } ->
          tick state pc stack env)
    | FPruning { pending = { pend_value = PendVal(_) } } -> ()
    | FPruning { pending = { pend_value = PendStopped } } -> assert false
    | FCall env' ->
      publish state stack' env' v
and halt state stack env =
  (* Stdio.eprintf "---HALT %s\n" ([%sexp_of: string list] (List.map stack ~f:(function
   *     | FOtherwise {instances; first_value} ->
   *       Printf.sprintf "(otherwise instances: %i; first_value: %b)" instances first_value
   *     | FPruning { instances } -> Printf.sprintf "(pruning instances: %i)" instances
   *     | FSequential _ -> "(seq)"
   *     | FResult -> "(res)"
   *     | FCall _ -> "(call)")
   *   )
   *                               |> Sexp.to_string_hum); *)
  let rec in_stack = function
    | [] -> ()
    | x::stack' -> match x with
      | FOtherwise {instances = 1 ;
                    first_value = false; pc } ->
        tick state pc stack' env
      | FOtherwise { instances = 1} ->
        in_stack stack'
      | FOtherwise r ->
        r.instances <- r.instances - 1
      | FPruning { instances = 1; pending = ({ pend_value = Pend } as pending)} ->
        pending.pend_value <- PendStopped
      | FPruning r ->
        r.instances <- r.instances - 1
      | _ -> in_stack stack' in
  in_stack stack
and tick
    ({ prims; instance } as state)
    (pc, c) stack env =
  (* print_debug state (pc, c) env; *)
  let realized arg =
    (match env.(arg) with
     | Pending ({ pend_value = Pend } as p) ->
       `Pending p
     | Pending { pend_value = PendStopped } ->
       `Stopped
     | Pending { pend_value = PendVal(v) }
     | Value(v) -> `Value v) in
  let realized_multi args =
    let values = (Array.create ~len:(Array.length args) (VConst (Ast.Null))) in
    let rec step = function
      | i when Int.equal (Array.length args) i -> `Values values
      | i ->
        match realized args.(i) with
        | `Pending p -> `Pending p
        | `Stopped -> `Stopped
        | `Value v ->
          values.(i) <- v;
          step (i + 1) in
    step 0 in
  let call_prim prim args =
    match realized_multi args with
    | `Pending p -> p.pend_waiters <- { pc = (pc, c); stack; env }::p.pend_waiters
    | `Stopped -> halt state stack env
    | `Values args' ->
      let impl = prims.(prim) in
      (* Stdio.eprintf "---CALL %i\n" prim;
       * Array.iter args' (fun v -> Stdio.eprintf "--ARG: %s\n" (sexp_of_v v |> Sexp.to_string_hum)); *)
      (match impl args' with
       | PrimVal res ->
         publish state stack env res;
         halt state stack env
       | PrimHalt -> halt state stack env
       | PrimUnsupported ->
         (* TODO warning? *)
         halt state stack env) in
  let (_, proc) = get_code state pc in
  match proc.(c) with
  | Const v ->
    publish state stack env (VConst v);
    halt state stack env
  | Label i ->
    publish state stack env (VLabel i);
    halt state stack env
  | Prim i ->
    publish state stack env (VPrim i);
    halt state stack env
  | Stop -> halt state stack env
  | Parallel(c1, c2) ->
    increment_instances stack;
    tick state (pc, c1) stack (Array.copy env);
    tick state (pc, c2) stack env;
  | Otherwise(c1, c2) ->
    let frame = FOtherwise { first_value = false;
                             instances = 1;
                             pc = (pc, c2); } in
    tick state (pc, c1) (frame::stack) env
  | Pruning(c1, i, c2) ->
    let pending = { pend_value = Pend; pend_waiters = [] } in
    let frame = FPruning { instances = 1;
                           pending;} in
    (match i with
     | None -> ()
     | Some i -> env.(i) <- Pending pending);
    tick state (pc, c2) (frame::stack) env;
    tick state (pc, c1) stack env
  | Sequential(c1, i, c2) ->
    let frame = FSequential(i, (pc, c2)) in
    tick state (pc, c1) (frame::stack) env
  | Closure (pc', to_copy) ->
    publish state stack env (VClosure(pc', to_copy, env));
    halt state stack env
  | Call(TPrim(prim), args) ->
    call_prim prim args
  | Call(TFun(pc'), args) ->
    let (size, f_code) = get_code state pc' in
    let env' = alloc_env size in
    let frame = FCall(env) in
    Array.iteri args ~f:(fun i arg ->
        env'.(i) <- env.(arg));
    tick state (pc', Array.length f_code - 1) (frame::stack) env'
  | TailCall(TFun(pc'), args) ->
    let (_, f_code) = get_code state pc' in
    Array.iteri args ~f:(fun i arg ->
        env.(i) <- env.(arg));
    let token = { pc = (pc', Array.length f_code - 1);
                  env = env;
                  stack = stack } in
    state.queue <- token::state.queue
  | Call(TDynamic(i), args) ->
    (match realized i with
     | `Pending p -> p.pend_waiters <- { pc = (pc, c); stack; env }::p.pend_waiters
     | `Stopped -> raise Util.TODO
     | `Value(VClosure(pc', to_copy, closure_env)) ->
       let (size, f_code) = get_code state pc' in
       let env' = alloc_env size in
       for i = 0 to to_copy - 1 do
         env'.(i) <- closure_env.(i)
       done;
       Array.iteri args ~f:(fun i arg ->
           env'.(i + to_copy) <- env.(arg));
       let frame = FCall(env) in
       tick state (pc', Array.length f_code - 1) (frame::stack) env'
     | `Value(VLabel(pc')) ->
       let (size, f_code) = get_code state pc' in
       let env' = alloc_env size in
       Array.iteri args ~f:(fun i arg ->
           env'.(i) <- env.(arg));
       let frame = FCall(env) in
       tick state (pc', Array.length f_code - 1) (frame::stack) env'
     | `Value(VTuple(vs)) ->
       (match realized args.(0) with
        | `Pending p -> p.pend_waiters <- { pc = (pc, c); stack; env }::p.pend_waiters
        | `Stopped -> raise Util.TODO
        | `Value(VConst(Ast.Int i)) ->
          publish state stack env (List.nth_exn vs i);
          halt state stack env
        | `Value(_) -> raise Util.TODO)
     | `Value(VPrim(prim)) ->
       call_prim prim args
     | `Value(_) -> raise Util.TODO)
  | Coeffect arg ->
    (match realized arg with
     | `Pending p ->
       p.pend_waiters <- { pc = (pc, c); stack; env }::p.pend_waiters
     | `Stopped -> raise Util.TODO
     | `Value(descr) ->
       let token = { pc = (pc, c); stack; env = Array.copy env } in
       state.coeffects (instance.current_coeffect, descr);
       instance.blocks <- (instance.current_coeffect, token)::instance.blocks;
       instance.current_coeffect <- instance.current_coeffect + 1)
  | TailCall(TDynamic(_), _) -> raise Util.TODO
  | TailCall(TPrim(_), _) -> assert false

let values_clb () =
  let storage = ref [] in
  (storage, (fun v -> storage:= v::!storage))

let coeffects_clb () =
  let storage = ref [] in
  (storage, (fun v -> storage:= v::!storage))

let rec run_loop state =
  match state.queue with
  | [] -> ()
  | {pc; env; stack}::xs ->
    state.queue <- xs;
    tick state pc stack env;
    run_loop state

let rec is_alive = function
  | [] -> true
  | FPruning { pending = { pend_value = PendStopped } }::_
  | FPruning { pending = { pend_value = PendVal(_)} }::_ ->
    false
  | _::xs -> is_alive xs

let check_killed just_unblocked instance =
  let f (killed, acc) ((id, {stack}) as block) =
    if Int.equal id just_unblocked
    then (killed, acc)
    else if is_alive stack
    then (killed, block::acc)
    else (id::killed, acc) in
  let (killed, blocks) = List.fold instance.blocks ~init:([], []) ~f in
  (killed, {instance with blocks})

let run' deps code =
  let instance = { current_coeffect = 0;
                   blocks = [] } in
  let (values, clb) = values_clb () in
  let (coeffects, c_clb) = coeffects_clb () in
  let (init_env_size, e_code) = code.(Array.length code - 1) in
  let pc = (-Array.length code, Array.length e_code - 1) in
  let env = alloc_env init_env_size in
  let stack = [FResult] in
  let state = { code; deps; instance;
                prims = Prims.default;
                values = clb;
                coeffects = c_clb;
                queue = [{pc; env; stack}]} in
  run_loop state;
  (* TODO killed coeffects *)
  Res.{ values = !values;
        coeffects = !coeffects;
        killed = [];
        instance}

let run ?(dependencies = [||]) code =
  Or_error.try_with ~backtrace:true (fun () -> run' dependencies code)

let unblock' deps code instance coeffect value =
  let (values, clb) = values_clb () in
  let (coeffects, c_clb) = coeffects_clb () in
  let state = { deps; code; instance;
                prims = Prims.default;
                values = clb;
                coeffects = c_clb;
                queue = [] } in
  match List.Assoc.find instance.blocks ~equal:Int.equal coeffect with
  | None -> Res.{ values = [];
                  coeffects = [];
                  killed = [];
                  instance}
  | Some(token) ->
    publish state token.stack token.env value;
    run_loop state;
    let (killed, instance') = check_killed coeffect instance in
    Res.{ values = !values;
          coeffects = !coeffects;
          killed;
          instance = instance'}

let unblock ?(dependencies = [||]) code instance coeffect value =
  Or_error.try_with ~backtrace:true (fun () -> unblock' dependencies code instance coeffect value)

let is_running { blocks } =
  not (List.is_empty blocks)
