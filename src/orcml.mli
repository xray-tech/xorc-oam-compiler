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
  and env
  [@@deriving sexp, compare]

  include Comparator.S with type t := t
end

type ir1 [@@deriving sexp_of]

type parse_error =
  [ | `NoInput
    | `SyntaxError of string * int * int]

type parse_value_error =
  [ parse_error
  | `UnsupportedValueAST ]

type no_deps_error =
  [ `UnexpectedDependencies of string list ]

type compile_error =
  [ `UnboundVar of string * Ast.pos
  | `UnknownReferedFunction of string * string ]

type inter_error =
  [ `UnknownFFI of string]

val error_to_string_hum : [< parse_error
                          | parse_value_error
                          | no_deps_error
                          | compile_error
                          | inter_error
                          | `BadFormat ] -> string

val parse : string -> (ast, [> parse_error]) Result.t

val parse_ns : filename:string -> string -> (ast, [> parse_error]) Result.t

val parse_value : string -> (Value.t, [> parse_value_error]) Result.t

val translate : ast -> string list * ir1

val translate_no_deps : ast -> (ir1, [> no_deps_error]) Result.t

type bc [@@deriving sexp_of, compare]

module Env : sig
  type prim_res = PrimVal of Value.t | PrimHalt | PrimUnsupported

  val register_ffi : string -> (Value.t array -> prim_res) -> unit
end

val compile : deps:(string * ir1) list -> ir1 -> (bc, [> compile_error]) Result.t

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

val run :  inter -> Res.t Or_error.t

val unblock : inter -> instance -> int -> Value.t -> Res.t Or_error.t

val is_running : instance -> bool

module Serializer : sig
  type load_error =
    [ | `BadFormat ]

  val dump : bc -> Msgpck.t
  val load : Msgpck.t -> (bc, [> load_error]) Result.t

  val dump_instance : instance -> Msgpck.t
  val load_instance : Msgpck.t -> (instance, [> load_error]) Result.t
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
