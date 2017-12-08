open Base

type ast [@@deriving sexp_of, compare]

type env

module Const : sig
  type t [@@deriving sexp_of, compare]
end

module Value : sig
  type t [@@deriving sexp_of, compare]

  include Comparator.S with type t := t
end

type ir1 [@@deriving sexp_of]

val parse : string -> ast Or_error.t

val parse_ns : string -> ast Or_error.t

val parse_value : string -> Value.t Or_error.t

val translate : ast -> (string list * ir1) Or_error.t

val translate_no_deps : ast -> ir1 Or_error.t

module Fun : sig
  type t = {
    ns : string;
    name : string;
  } [@@deriving sexp_of, compare]
  type impl
end

type imports = (int * Fun.t) list
type repo = (string * Fun.impl) list [@@deriving sexp_of, compare]

val compile : ir1 -> (imports * repo) Or_error.t

val compile_ns : ir1 -> (imports * repo) Or_error.t

type linker = int -> int Or_error.t

val link : repo -> linker -> repo Or_error.t

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

module Serializer : sig
  val dump : bc -> Msgpck.t
  val load : Msgpck.t -> bc Or_error.t

  val dump_instance : instance -> Msgpck.t
  val load_instance : Msgpck.t -> instance Or_error.t
end

module Testkit : sig
  type msg = Execute of bc
           | Continue of int * Value.t
           | Benchmark of bc * int
  type res = { values : Value.t list;
               coeffects : coeffect list;
               killed : int list }
  module Serializer : sig
    val dump_msg : msg -> Msgpck.t
    val load_msg : Msgpck.t -> msg Or_error.t

    val dump_res : Res.t -> Msgpck.t
    val load_res : Msgpck.t -> res Or_error.t

    val dump_bench_res : float -> Msgpck.t
    val load_bench_res : Msgpck.t -> float Or_error.t
  end
end
