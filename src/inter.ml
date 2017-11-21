open Base
type call_target = TPrim of int | TFun of int | TClosure of int [@@deriving sexp, compare]

type c = int [@@deriving sexp, compare]

type t =
  | Parallel of c * c
  | Otherwise of c * c
  | Pruning of c * int option * c
  | Sequential of c * int option * c
  | Call of call_target * int array
  | TailCall of call_target * int array
  | Coeffect of int
  | Stop
  | Const of Ast.const
  | Closure of (int * int)
and v =
  | VConst of Ast.const
  | VClosure of int * int * env
  (* | VTuple of v list
   * | VList of v list
   * | VRecord of (string * v) list *)
and env_v =
  | Value of v | Pending of pending
and env = env_v array
and pend_value = PendVal of v | PendStopped | Pend
and pending = {
  mutable pend_value : pend_value;
  mutable pend_waiters : token list;
}
and frame =
  | FPruning of { mutable instances : int;
                  pending : pending }
  | FOtherwise of { mutable first_value : bool;
                    mutable instances : int;
                    pc : (int * c) }
  | FSequential of (int option * (int * c))
  | FCall of env
  | FResult
and stack = frame list
and token = {
  pc : (int * c);
  env : env;
  stack : stack;
} [@@deriving sexp, compare]

module Value = struct
  include (val Comparator.make ~compare:compare_v ~sexp_of_t:sexp_of_v)
end

type code = (int * t array) array [@@deriving sexp]
type instance = { mutable current_coeffect : int;
                  mutable blocks : (int * token) list }

type prim_v = PrimVal of v | PrimHalt | PrimUnsupported
type prims = (v array -> prim_v) array

type state = { code : code;
               instance : instance;
               prims : prims;
               values: (v -> unit);
               coeffects: ((int * v) -> unit)}

exception TODO
exception RuntimeError

let default_prims = [|
  (* Let *)
  (function
    | [| VConst _ as v |] -> PrimVal v
    | _ -> PrimUnsupported);
  (* Add *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Int(x + y)))
    | [| VConst(Ast.String x); VConst(Ast.String y) |] -> PrimVal (VConst(Ast.String(String.concat [x;y])))
    | _ -> PrimUnsupported);
  (* Sub *)
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Int(x - y)))
    | _ -> PrimUnsupported);
  (* Ift *)
  (function
    | [| VConst(Ast.Bool true) |] -> PrimVal (VConst(Ast.Signal))
    | [| VConst(Ast.Bool false) |] -> PrimHalt
    | _ -> PrimUnsupported);
  (* Iff *)
  (function
    | [| VConst(Ast.Bool false) |] -> PrimVal (VConst(Ast.Signal))
    | [| VConst(Ast.Bool true) |] -> PrimHalt
    | _ -> PrimUnsupported);
  (* Eq *)
  (function
    | [| VConst(v1); VConst(v2) |] -> PrimVal (VConst(Ast.Bool (Polymorphic_compare.equal v1 v2)))
    | _ -> PrimUnsupported)
|]

let rec increment_instances = function
  | [] -> ()
  | FOtherwise(r)::_ -> r.instances <- r.instances + 1
  | FPruning(r)::_ -> r.instances <- r.instances + 1
  | x::xs -> increment_instances xs

let alloc_env size = Array.create size (Value (VConst Ast.Null))

(* TODO should be local exception  *)
exception ToWait
exception ToStop

let rec format_value = function
  | Value v -> format_v v
  | Pending { pend_value = PendVal v} -> Printf.sprintf "(Pending %s)" (format_v v)
  | Pending { pend_value = v} -> sexp_of_pend_value v |> Sexp.to_string_hum
and format_v = function
  | VConst v  -> Ast.sexp_of_const v |> Sexp.to_string_hum
  | VClosure (i, _, _) -> Printf.sprintf "(Closure %i)" i

let format_env env =
  let vs = (Array.to_sequence env
            |> Sequence.map ~f:format_value
            |> Sequence.to_list) in
  "(" ^ String.concat ~sep:", " vs ^ ")"

let print_debug { code } (pc, c) env  =
  let (size, f) = code.(pc) in
  let op = f.(c) in
  Stdio.printf "Op (%i:%i:size %i): %s Env: %s\n"
    pc c size
    (sexp_of_t op |> Sexp.to_string_hum)
    (format_env env)

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
  let rec in_stack = function
    | [] -> ()
    | x::stack' -> match x with
      | FOtherwise {instances =  1 ;
                    first_value = false; pc } ->
        tick state pc stack' env
      | FOtherwise { instances = 1 } ->
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
    ({ code; prims; instance } as state)
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
    let values = (Array.create (Array.length args) (VConst (Ast.Null))) in
    let rec step i =
      match realized args.(i) with
      | `Pending p -> `Pending p
      | `Stopped -> `Stopped
      | `Value v ->
        values.(i) <- v;
        if i + 1 < Array.length args
        then step (i + 1)
        else `Values values in
    step 0 in
  let (_, proc) = code.(pc) in
  match proc.(c) with
  | Const v -> publish state stack env (VConst v)
  | Stop -> halt state stack env
  | Parallel(c1, c2) ->
    increment_instances stack;
    tick state (pc, c1) stack env;
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
    publish state stack env (VClosure(pc', to_copy, env))
  | Call(TPrim(prim), args) ->
    (match realized_multi args with
     | `Pending p -> p.pend_waiters <- { pc = (pc, c); stack; env }::p.pend_waiters
     | `Stopped -> halt state stack env
     | `Values args' ->
       let impl = prims.(prim) in
       (match impl args' with
        | PrimVal res ->
          publish state stack env res
        | PrimHalt -> halt state stack env
        | PrimUnsupported -> raise RuntimeError))
  | Call(TFun(pc'), args) ->
    let (size, f_code) = code.(pc') in
    let env' = alloc_env size in
    let frame = FCall(env) in
    Array.iteri args ~f:(fun i arg ->
        env'.(i) <- env.(arg));
    tick state (pc', Array.length f_code - 1) (frame::stack) env'
  | Call(TClosure(i), args) ->
    (match realized i with
     | `Pending p -> p.pend_waiters <- { pc = (pc, c); stack; env }::p.pend_waiters
     | `Stopped -> raise Util.TODO
     | `Value(VConst _) -> raise RuntimeError
     | `Value(VClosure(pc', to_copy, closure_env)) ->
       let (size, f_code) = code.(pc') in
       let env' = alloc_env size in
       for i = 0 to to_copy - 1 do
         env'.(i) <- closure_env.(i)
       done;
       Array.iteri args ~f:(fun i arg ->
           env'.(i + to_copy) <- env.(arg));
       let frame = FCall(env) in
       tick state (pc', Array.length f_code - 1) (frame::stack) env')
  | Coeffect arg ->
    (match realized arg with
     | `Pending p -> p.pend_waiters <- { pc = (pc, c); stack; env }::p.pend_waiters
     | `Stopped -> raise Util.TODO
     | `Value(descr) ->
       let token = { pc = (pc, c); stack; env } in
       state.coeffects (instance.current_coeffect, descr);
       instance.blocks <- (instance.current_coeffect, token)::instance.blocks;
       instance.current_coeffect <- instance.current_coeffect + 1)
  | _ -> raise TODO

let values_clb () =
  let storage = ref [] in
  (storage, (fun v -> storage:= v::!storage))

let coeffects_clb () =
  let storage = ref [] in
  (storage, (fun v -> storage:= v::!storage))

let run code =
  let instance = { current_coeffect = 0;
                   blocks = [] } in
  let (values, clb) = values_clb () in
  let (coeffects, c_clb) = coeffects_clb () in
  let state = { code; instance;
                prims = default_prims;
                values = clb;
                coeffects = c_clb} in
  let (init_env_size, e_code) = code.(Array.length code - 1) in
  let pc = (Array.length code - 1, Array.length e_code - 1) in
  let env = alloc_env init_env_size in
  let stack = [FResult] in
  tick state pc stack env;
  (* TODO killed coeffects *)
  (!values, !coeffects, [], instance)

let unblock code instance coeffect value =
  let (values, clb) = values_clb () in
  let (coeffects, c_clb) = coeffects_clb () in
  let state = { code; instance;
                prims = default_prims;
                values = clb;
                coeffects = c_clb} in
  let token = List.Assoc.find_exn instance.blocks ~equal:Int.equal coeffect in
  publish state token.stack token.env value;
  (!values, !coeffects, [], instance)
