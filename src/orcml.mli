open Base

type ast [@@deriving sexp_of, compare]

type env

module Const : sig
  type t =
    | Int of int
    | Float of float
    | String of string
    | Signal
    | Null
    | Bool of bool
  [@@deriving sexp_of, compare]
end

module Value : sig
  type t =
    | VConst of Const.t
    | VClosure of int * int * env
    | VLabel of int
    | VTuple of t list
    | VList of t list
    | VRecord of (string * t) list
    | VRef of t ref
    | VPending of pending
  and env
  and pending
  [@@deriving sexp, compare]

  include Comparator.S with type t := t
end

type pos = {
  path : string;
  line : int;
  col : int;
}
and range = {
  start : pos;
  finish : pos;
} [@@deriving sexp, compare]

type parse_error =
  [ | `NoInput
    | `SyntaxError of pos * string]

type parse_value_error =
  [ parse_error
  | `UnsupportedValueAST ]

type compile_error =
  [ `UnboundVar of range * string
  | `UnknownReferedFunction of string * string ]

type error = [ parse_error | compile_error ]

type inter_error =
  [ `UnknownFFI of string]

val error_to_string_hum : [< parse_error
                          | parse_value_error
                          | compile_error
                          | inter_error
                          | `BadFormat ] -> string


type bc [@@deriving sexp_of, compare]

module Env : sig
  type prim_res =
    | PrimVal of Value.t
    | PrimHalt
    | PrimUnsupported
    | PrimPendingSubscribe of Value.pending
    | PrimPendingRealize of (Value.pending * Value.t)
    | PrimPendingStop of Value.pending

  val register_ffi : string -> (Value.t array -> prim_res) -> unit
end

module Repository : sig
  type t

  val create : unit -> t
end

val parse_value : string -> (Value.t, [> parse_value_error]) Result.t

val compile : ?prelude:(string * string) list -> repository:Repository.t -> string -> (bc, [> error]) Result.t

val add_module : repository:Repository.t -> path:string -> string -> (unit, [> error]) Result.t

type coeffect = int * Value.t
type instance

module Res : sig
  type t = { values : Value.t list;
             coeffects : coeffect list;
             killed : int list;
             instance : instance}
end

type inter

val inter : bc -> (inter, [> inter_error]) Result.t

val run :  inter -> Res.t

val unblock : inter -> instance -> int -> Value.t -> Res.t

val is_running : instance -> bool

module Serializer : sig
  type load_error =
    [ | `BadFormat ]

  val dump : bc -> Msgpck.t
  val load : Msgpck.t -> (bc, [> load_error]) Result.t

  val dump_instance : instance -> Msgpck.t
  val load_instance : Msgpck.t -> (instance, [> load_error]) Result.t

  val dump_k : bc -> string
end

module Testkit : sig
  type msg = Execute of bc
           | Continue of int * Value.t
           | Benchmark of bc * int
  type res = { values : Value.t list;
               coeffects : coeffect list;
               killed : int list }
  module Serializer : sig
    type load_error = [`BadFormat]
    val dump_msg : msg -> Msgpck.t
    val load_msg : Msgpck.t -> (msg, [> load_error]) Result.t

    val dump_res : Res.t -> Msgpck.t
    val load_res : Msgpck.t -> (res, [> load_error]) Result.t

    val dump_bench_res : float -> Msgpck.t
    val load_bench_res : Msgpck.t -> (float, [> load_error]) Result.t
  end
end



module Debugger : sig
  type op
  type stack
  type state
  type thread = { id : int;
                  op : op;
                  env : Value.t array;
                  stack : stack;
                  pos : pos}
  type threads = thread list
  type action =
    | PublishedValue of Value.t
    | NewThread of int
    | HaltedThread of int
    | Coeffect of { thread : int; id : int; desc : Value.t }
    | Error of { thread : int; ffi : string; args : Value.t list}

  type trace = action list
  val init : inter -> (state * threads)
  val tick : state -> thread -> (threads * trace)
  (* val unblock : state -> (state * threads * trace) *)
end
