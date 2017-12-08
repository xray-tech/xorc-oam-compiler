open Base
type call_target = TPrim of int | TFun of int | TDynamic of int [@@deriving sexp, compare]

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
  | Label of int
  | Prim of int
and v =
  | VConst of Ast.const
  | VClosure of int * int * env
  | VLabel of int
  | VPrim of int
  | VTuple of v list
  | VList of v list
  | VRecord of (string * v) list
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

type prim_v = PrimVal of v | PrimHalt | PrimUnsupported
type prims = (v array -> prim_v) array
