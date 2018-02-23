open Base
type parse_error = Errors.parse_error

type parse_value_error = Errors.parse_value_error

type compile_error = Errors.compile_error

type inter_error = Errors.inter_error

type error = [ parse_error | compile_error ]

let error_to_string_hum = Errors.to_string_hum

type ast = Ast.e
let sexp_of_ast = Ast.sexp_of_e
let compare_ast = Ast.compare_e

type env = Inter.env
module Const = struct
  type t = Ast.const =
    | Int of int
    | Float of float
    | String of string
    | Signal
    | Null
    | Bool of bool
  [@@deriving sexp_of, compare]
  let compare = Ast.compare_const
  let sexp_of_t = Ast.sexp_of_const
end

module Value = struct
  type t = Inter.v =
    | VConst of Const.t
    | VClosure of int * int * env
    | VLabel of int
    | VTuple of t list
    | VList of t list
    | VRecord of (string * t) list
    | VRef of t ref
    | VPending of pending
  and env = Inter.env
  and pending = Inter.pending
  let sexp_of_t = Inter.sexp_of_v
  let t_of_sexp = Inter.v_of_sexp
  let compare = Inter.compare_v

  let sexp_of_env = Inter.sexp_of_env
  let env_of_sexp = Inter.env_of_sexp
  let compare_env = Inter.compare_env

  let sexp_of_pending = Inter.sexp_of_pending
  let pending_of_sexp = Inter.pending_of_sexp
  let compare_pending = Inter.compare_pending

  include Inter.Value
end

module Repository = Repository

let parse_value = Syntax.parse_value

let compile ?(prelude = []) ~repository code =
  let open Result.Let_syntax in
  let%bind (parsed, comments) = Syntax.parse code in
  let (_, ir1) = Ir1.translate parsed in
  Compiler.compile ~prelude ~comments ~repository ir1

let compile_module ~repository ~name code =
  let open Result.Let_syntax in
  let%map (parsed, comments) = Syntax.parse_module ~filename:name code in
  let (_, ir1) = Ir1.translate parsed in
  Compiler.compile_module ~comments ~repository ~name ir1

type bc = Inter.bc
let sexp_of_bc = Inter.sexp_of_bc
let compare_bc = Inter.compare_bc

type coeffect = Inter.coeffect
type instance = Inter.instance

module Res = Inter.Res

module Env = Env

type inter = Inter.inter

let inter = Inter.inter

let run = Inter.run
let unblock = Inter.unblock
let is_running = Inter.is_running

module Serializer = Serializer

module Testkit = Testkit
