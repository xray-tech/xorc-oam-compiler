open Base

type ast [@@deriving sexp_of, compare]

type env

module Const : sig
  type t [@@deriving sexp_of, compare]
end

module Value : sig
  type t [@@deriving sexp, compare]

  include Comparator.S with type t := t
end

type ir1 [@@deriving sexp_of]

type parse_error =
  [ | `NoInput
    | `SyntaxError of string * int * int]

type parse_value_error =
  [ parse_error
  | `UnsupportedValueAST ]

type 'a link_error =
  [ `LinkerError of 'a]

type no_deps_error =
  [ `UnexpectedDependencies of string list ]

type compile_error =
  [ `UnboundVar of string * Ast.pos ]

val error_to_string_hum : [< parse_error
                          | parse_value_error
                          | no_deps_error
                          | compile_error
                          | `BadFormat ] -> string

val parse : string -> (ast, [> parse_error]) Result.t

val parse_ns : string -> (ast, [> parse_error]) Result.t

val parse_value : string -> (Value.t, [> parse_value_error]) Result.t

val translate : ast -> string list * ir1

val translate_no_deps : ast -> (ir1, [> no_deps_error]) Result.t

module Fun : sig
  type t = {
    ns : string;
    name : string;
  } [@@deriving sexp_of, compare]
  type impl
end

type imports = (int * Fun.t) list
type repo = (string * Fun.impl) list [@@deriving sexp_of, compare]

val compile : ir1 -> (imports * repo, [> compile_error]) Result.t

val compile_ns : ir1 -> (imports * repo, [> compile_error]) Result.t

type 'a linker = int -> (int, 'a) Result.t

val link : repo -> 'a linker -> (repo, [> 'a link_error]) Result.t

type bc [@@deriving sexp_of, compare]

val finalize : repo -> bc

type coeffect = int * Value.t
type instance

module Res : sig
  type t = { values : Value.t list;
             coeffects : coeffect list;
             killed : int list;
             instance : instance}
end

val run : ?dependencies:bc -> bc -> Res.t Or_error.t

val unblock : ?dependencies:bc -> bc -> instance -> int -> Value.t -> Res.t Or_error.t

val is_running : instance -> bool

module Serializer : sig
  type 'a load_error =
    [ | 'a link_error
      | `BadFormat ]
  val imports : Msgpck.t -> (imports, [> `BadFormat]) Result.t

  val dump : ?imports:imports -> bc -> Msgpck.t
  val load : ?linker:('a linker) -> Msgpck.t -> (bc, [> 'a load_error]) Result.t

  val dump_instance : ?mapping:(Fun.t * int) list -> instance -> Msgpck.t
  val load_instance : ?linker:('a linker) -> Msgpck.t -> (instance, [> 'a load_error]) Result.t
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
