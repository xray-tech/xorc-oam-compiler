open Base
type call_target = TPrim of int | TFun of int | TClosure of int [@@deriving sexp]

type c = int [@@deriving sexp]

type t =
  | Parallel of c * c
  | Otherwise of c * c
  | Pruning of c * int * c
  | Sequential of c * int option * c
  | Stop
  | Const of Ast.const
  | Closure of int
  | Call of call_target * int array
  | TailCall of call_target * int array
  | Coeffect of string
and v =
  | VConst of Ast.const
  | VClosure of int * env
and env_v =
  | Value of v | Pending of pending
and env = env_v array
and pending = {
  mutable pend_value : v option;
  mutable pend_waiters : token list;
}
and frame =
  | FPruning of { mutable realized : bool;
                  mutable instances : int;
                  pending : pending }
  | FOtherwise of { mutable first_value : bool;
                    mutable instances : int;
                    pc : (int * c) }
  | FSequential of (int * c)
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

type prims = (v array -> v option) array

type state = { code : code;
               instance : instance;
               prims : prims }

exception TODO
exception RuntimeError

let default_prims = [|
  (function
    | [| VConst _ as v |] -> Some v
    | _ -> None);
  (function
    | [| VConst(Ast.Int x); VConst(Ast.Int y) |] -> Some(VConst(Ast.Int(x + y)))
    | [| VConst(Ast.String x); VConst(Ast.String y) |] -> Some(VConst(Ast.String(String.concat [x;y])))
    | _ -> None)
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

let rec publish state ({ stack } as token) v =
  let rec in_stack = function
    | [] -> assert false
    | x::stack' -> match x with
      | FResult clb -> clb v
      | FSequential pc ->
        increment_instances token;
        token.stack <- stack';
        token.pc <- pc;
        tick state (token_copy_env token)
      | FOtherwise ({ first_value = false } as r) ->
        r.first_value <- true;
        in_stack stack'
      | FOtherwise _ ->
        in_stack stack'
      | FPruning ({ realized = false; pending } as r) ->
        r.realized <- true;
        pending.pend_value <- Some v;
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
      | FPruning { instances = 1; realized = false} ->
        in_stack stack'
      | FPruning r ->
        r.instances <- r.instances - 1
      | _ -> in_stack stack' in
  in_stack stack
and tick
    ({ code; prims; instance } as state)
    ({ pc = (pc, c); env; stack } as token) =
  let (_, proc) = code.(pc) in
  match proc.(c) with
  | Const v -> publish state token (VConst v)
  | Stop -> halt state token
  | Parallel(c1, c2) ->
    increment_instances token;
    token.pc <- (pc, c1);
    tick state token;
    let token2 = token_copy_env token in
    token2.pc <- (pc, c2);
    tick state token2
  | Otherwise(c1, c2) ->
    let frame = FOtherwise { first_value = false;
                             instances = 1;
                             pc = (pc, c2); } in
    token.pc <- (pc, c1);
    token.stack <- frame::stack;
    tick state token
  | Pruning(c1, i, c2) ->
    let pending = { pend_value = None; pend_waiters = [] } in
    let frame = FPruning { realized = false;
                           instances = 1;
                           pending;} in
    token.env.(i) <- Pending pending;
    token.stack <- frame::stack;
    token.pc <- (pc, c2);
    tick state token;
    token.stack <- stack;
    token.pc <- (pc, c1);
    tick state token
  | Sequential(c1, i, c2) ->
    let frame = FSequential((pc, c2)) in
    token.stack <- frame::stack;
    token.pc <- (pc, c1);
    tick state token
  | Closure pc' ->
    publish state token (VClosure(pc', Array.copy env))
  | Call(TPrim(prim), args) ->
    let impl = prims.(prim) in
    (match (try Some(Array.map args ~f:(fun i ->
         match env.(i) with
         | Pending ({ pend_value = None } as p) ->
           p.pend_waiters <- token::p.pend_waiters;
           raise ToWait
         | Pending { pend_value = Some(v) }
         | Value v -> v)) with
       | ToWait -> None) with
     | None -> ()
     | Some args' ->
       match impl args' with
       | Some(res) ->
         publish state token res
       | None -> raise TODO)
  | Call(TFun(pc'), args) ->
    let (size, f_code) = code.(pc') in
    let env' = alloc_env size in
    let frame = FCall(token.env) in
    Array.iter args (fun i ->
        env'.(i) <- env.(i));
    token.env <- env';
    token.stack <- frame::stack;
    token.pc <- (pc', Array.length f_code - 1);
    tick state token
  | Call(TClosure(i), args) ->
    (match env.(i) with
     | Pending p->
       p.pend_waiters <- token::p.pend_waiters
     | Value(VConst _) -> raise RuntimeError
     | Value(VClosure(pc', closure_env)) ->
       let (size, _) = code.(pc') in
       let env' = alloc_env size in
       for i = 0 to Array.length closure_env do
         env'.(i) <- closure_env.(i)
       done;
       let frame = FCall(token.env) in
       token.env <- env';
       token.stack <- frame::stack;
       token.pc <- (pc', 0);
       tick state token)
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
