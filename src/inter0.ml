open Base
type call_target = TFun of int | TDynamic of int [@@deriving sexp, compare]

type c = int [@@deriving sexp, compare]

type pos = unit [@@deriving sexp, compare]

type op = (int * c) [@@deriving sexp, compare]

type t' =
  | Parallel of c * c
  | Otherwise of c * c
  | Pruning of c * int option * c
  | Sequential of c * int option * c
  | Call of call_target * int array
  | FFI of int * int array
  | TailCall of call_target * int array
  | Coeffect of int
  | Stop
  | Const of Ast.const
  | Closure of (int * int)
  | Label of int
and t = (t' * Ast.pos)
and v =
  | VConst of Ast.const
  | VClosure of int * int * env
  | VLabel of int
  | VTuple of v list
  | VList of v list
  | VRecord of (string * v) list
  | VRef of v ref
  | VPending of pending
and env_v =
  | Value of v | Pending of pending
and env = env_v array
and pend_value = PendVal of v | PendStopped | Pend
and pending = {
  mutable pend_value : pend_value;
  mutable pend_waiters : thread list;
}
and frame =
  | FPruning of { mutable instances : int;
                  pending : pending }
  | FOtherwise of { mutable first_value : bool;
                    mutable instances : int;
                    op : op }
  | FSequential of (int option * op)
  | FCall of env
  | FResult
and stack = frame list
and thread = {
  id : int;
  op : op;
  env : env;
  stack : stack;
  pos : pos
} [@@deriving sexp, compare]

type prim_v =
  | PrimVal of v
  | PrimHalt
  | PrimUnsupported
  | PrimPendingSubscribe of pending
  | PrimPendingRealize of (pending * v)
  | PrimPendingStop of pending

type prims = (v array -> prim_v) array

type code = (int * t array) array
[@@deriving sexp_of, compare]

type bc = {
  ffi : string list;
  code : code;
} [@@deriving sexp_of, compare]

let rec format_value = function
  | Value v -> format_v v
  | Pending p -> format_pending p
and format_pending = function
  | { pend_value = PendVal v} -> Printf.sprintf "(Pending %s)" (format_v v)
  | { pend_value = v} -> sexp_of_pend_value v |> Sexp.to_string_hum
and format_v = function
  | VConst v  -> Ast.sexp_of_const v |> Sexp.to_string_hum
  | VTuple l -> Printf.sprintf "(Tuple %s)" (String.concat ~sep:", " (List.map l ~f:format_v))
  | VList l -> Printf.sprintf "(List %s)" (String.concat ~sep:", " (List.map l ~f:format_v))
  | VRecord pairs -> Printf.sprintf "(Record %s)" (String.concat ~sep:", " (List.map pairs ~f:(fun (k, v) -> Printf.sprintf "%s: %s" k (format_v v))))
  | VClosure (i, _, _) -> Printf.sprintf "(Closure %i)" i
  | VLabel i -> Printf.sprintf "(Label %i)" i
  | VRef v -> Printf.sprintf "(Ref %s)" (format_v !v)
  | VPending p -> format_pending p

let format_env env =
  let vs = (Array.to_sequence env
            |> Sequence.map ~f:format_value
            |> Sequence.to_list) in
  "(" ^ String.concat ~sep:", " vs ^ ")"
