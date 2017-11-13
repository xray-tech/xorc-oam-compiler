open Base
type call_target = TPrim of int | TFun of int | TClosure of int [@@deriving sexp]

type c = int [@@deriving sexp]

type t =
  | Parallel of c * c
  | Otherwise of c * c
  | Pruning of c * int option * c
  | Sequential of c * int option * c
  | Stop
  | Const of Ast.const
  | Closure of (int * int)
  | Call of call_target * int array
  | TailCall of call_target * int array
  | Coeffect of string
and v =
  | VConst of Ast.const
  | VClosure of int * int * env
and env_v =
  | Value of v | Pending of pending
and env = env_v array
and pend_value = PendVal of v | PendStopped | Pend
and pending = {
  mutable pend_value : pend_value;
  mutable pend_waiters : token list;
}
and frame =
  | FPruning of { mutable realized : bool;
                  mutable instances : int;
                  pending : pending }
  | FOtherwise of { mutable first_value : bool;
                    mutable instances : int;
                    pc : (int * c) }
  | FSequential of (int option * (int * c))
  | FCall of env
  | FResult of (v -> unit)
and stack = frame list
and token = {
  mutable pc : (int * c);
  mutable env : env;
  mutable stack : stack;
} [@@deriving sexp]

type code = (int * t array) array [@@deriving sexp]
type instance = { mutable current_coeffect : int;
                  mutable blocks : (int * token) list }

type prim_v = PrimVal of v | PrimHalt | PrimUnsupported
type prims = (v array -> prim_v) array

type state = { code : code;
               instance : instance;
               prims : prims }

exception TODO
exception RuntimeError

let default_prims = [|
  (function
    | [| VConst _ as v |] -> PrimVal v
    | _ -> PrimUnsupported);
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Int(x + y)))
    | [| VConst(Ast.String x); VConst(Ast.String y) |] -> PrimVal (VConst(Ast.String(String.concat [x;y])))
    | _ -> PrimUnsupported);
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> PrimVal (VConst(Ast.Int(x - y)))
    | _ -> PrimUnsupported);
  (function
    | [| VConst(Ast.Bool true) |] -> PrimVal (VConst(Ast.Signal))
    | [| VConst(Ast.Bool false) |] -> PrimHalt
    | _ -> PrimUnsupported);
  (function
    | [| VConst(Ast.Bool false) |] -> PrimVal (VConst(Ast.Signal))
    | [| VConst(Ast.Bool true) |] -> PrimHalt
    | _ -> PrimUnsupported);
  (function
    | [| VConst(v1); VConst(v2) |] -> PrimVal (VConst(Ast.Bool (Polymorphic_compare.equal v1 v2)))
    | _ -> PrimUnsupported)
|]

let increment_instances { stack } =
  let rec incr = function
    | [] -> ()
    | FOtherwise(r)::_ -> r.instances <- r.instances + 1
    | FPruning(r)::_ -> r.instances <- r.instances + 1
    | x::xs -> incr xs in
  incr stack

let token_copy_env token =
  { token with env = Array.copy token.env }

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

let print_token { code } { pc = (pc, c); env; stack; } =
  let (size, f) = code.(pc) in
  let op = f.(c) in
  Stdio.printf "Op (%i:%i:size %i): %s Env: %s\n"
    pc c size
    (sexp_of_t op |> Sexp.to_string_hum)
    (format_env env)


let rec publish state ({ stack } as token) v =
  let rec in_stack = function
    | [] -> assert false
    | x::stack' -> match x with
      | FResult clb -> clb v
      | FSequential(i, pc) ->
        increment_instances token;
        let token' = (token_copy_env token) in
        token'.stack <- stack';
        token'.pc <- pc;
        (match i with
         | None -> ()
         | Some i ->
           token'.env.(i) <- Value(v));
        tick state token'
      | FOtherwise ({ first_value = false } as r) ->
        r.first_value <- true;
        in_stack stack'
      | FOtherwise _ ->
        in_stack stack'
      | FPruning ({ realized = false; pending } as r) ->
        r.realized <- true;
        (* (match v with
         *  | VClosure _ -> ()
         *  | v -> sexp_of_v v |> Sexp.to_string_hum |> Stdio.printf "---PruningVAL: %s\n"); *)
        pending.pend_value <- PendVal v;
        List.iter pending.pend_waiters ~f:(fun token ->
            tick state token)
      | FPruning { realized = true } -> ()
      | FCall env ->
        token.env <- env;
        in_stack stack' in
  in_stack stack
and halt state ({ stack; pc = (pc, _) } as token) =
  let rec in_stack = function
    | [] -> ()
    | x::stack' -> match x with
      | FOtherwise {instances =  1 ;
                    first_value = false; pc } ->
        token.pc <- pc;
        token.stack <- stack';
        tick state token
      | FOtherwise { instances = 1 } ->
        in_stack stack'
      | FOtherwise r ->
        r.instances <- r.instances - 1
      | FPruning { instances = 1; realized = false; pending} ->
        pending.pend_value <- PendStopped
      | FPruning r ->
        r.instances <- r.instances - 1
      | _ -> in_stack stack' in
  in_stack stack
and tick
    ({ code; prims; instance } as state)
    ({ pc = (pc, c); env; stack } as token) =
  (* print_token state token; *)
  let (_, proc) = code.(pc) in
  match proc.(c) with
  | Const v -> publish state token (VConst v)
  | Stop -> halt state token
  | Parallel(c1, c2) ->
    increment_instances token;
    token.pc <- (pc, c1);
    tick state token;
    token.pc <- (pc, c2);
    tick state token
  | Otherwise(c1, c2) ->
    let frame = FOtherwise { first_value = false;
                             instances = 1;
                             pc = (pc, c2); } in
    token.pc <- (pc, c1);
    token.stack <- frame::stack;
    tick state token
  | Pruning(c1, i, c2) ->
    let pending = { pend_value = Pend; pend_waiters = [] } in
    let frame = FPruning { realized = false;
                           instances = 1;
                           pending;} in
    (match i with
     | None -> ()
     | Some i -> token.env.(i) <- Pending pending);
    token.stack <- frame::stack;
    token.pc <- (pc, c2);
    tick state token;
    token.stack <- stack;
    token.pc <- (pc, c1);
    tick state token
  | Sequential(c1, i, c2) ->
    let frame = FSequential(i, (pc, c2)) in
    token.stack <- frame::stack;
    token.pc <- (pc, c1);
    tick state token
  | Closure (pc', to_copy) ->
    publish state token (VClosure(pc', to_copy, env))
  | Call(TPrim(prim), args) ->
    (* Stdio.printf "---PRIM CALL %i\n" prim; *)
    let impl = prims.(prim) in
    (match (try `Args (Array.map args ~f:(fun i ->
         match env.(i) with
         | Pending ({ pend_value = Pend } as p) ->
           p.pend_waiters <- token::p.pend_waiters;
           raise ToWait
         | Pending { pend_value = PendStopped } ->
           raise ToStop
         | Pending { pend_value = PendVal(v) }
         | Value v -> v)) with
       | ToWait -> `Wait
       | ToStop -> `Stop) with
     | `Stop -> halt state token
     | `Wait -> ()
     | `Args args' ->
       (* [%sexp_of: v array] args' |> Sexp.to_string_hum |> Stdio.printf "---ARGS: %s\n"; *)
       match impl args' with
       | PrimVal res ->
         publish state token res
       | PrimHalt -> halt state token
       | PrimUnsupported -> raise RuntimeError)
  | Call(TFun(pc'), args) ->
    let (size, f_code) = code.(pc') in
    let env' = alloc_env size in
    let frame = FCall(token.env) in
    Array.iteri args (fun i arg ->
        env'.(i) <- env.(arg));
    token.env <- env';
    token.stack <- frame::stack;
    token.pc <- (pc', Array.length f_code - 1);
    tick state token
  | Call(TClosure(i), args) ->
    (match env.(i) with
     | Pending ({ pend_value = Pend } as p) ->
       p.pend_waiters <- token::p.pend_waiters;
     | Pending { pend_value = PendStopped } ->
       raise Util.TODO
     | Value(VConst _) -> raise RuntimeError
     | Pending { pend_value = PendVal(VClosure(pc', to_copy, closure_env)) }
     | Value(VClosure(pc', to_copy, closure_env)) ->
       let (size, f_code) = code.(pc') in
       let env' = alloc_env size in
       (* Stdio.printf "----ALL: size: %i; closure_sizr: %i\n" size (Array.length closure_env); *)
       for i = 0 to to_copy - 1 do
         env'.(i) <- closure_env.(i)
       done;
       (* Stdio.printf "----ARGS size: %i; env_size: %i\n" (Array.length args) (Array.length env); *)
       Array.iteri args (fun i arg ->
           env'.(i + to_copy) <- env.(arg));
       let frame = FCall(token.env) in
       token.env <- env';
       token.stack <- frame::stack;
       token.pc <- (pc', Array.length f_code - 1);
       tick state token
     | Pending { pend_value = PendVal(_) } ->
       raise RuntimeError)
  | Coeffect descr ->
    instance.blocks <- (instance.current_coeffect, token)::instance.blocks;
    instance.current_coeffect <- instance.current_coeffect + 1
  | _ -> raise TODO

let run code clb =
  let instance = { current_coeffect = 0;
                   blocks = [] } in
  let state = { code; instance; prims = default_prims } in
  let (init_env_size, e_code) = code.(Array.length code - 1) in
  let token = { pc = (Array.length code - 1, Array.length e_code - 1);
                env = alloc_env init_env_size;
                stack = [(FResult clb)]} in
  tick state token
