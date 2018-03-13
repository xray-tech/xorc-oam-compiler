open Base

type pos = Ast.Pos.t = {
  path : string;
  line : int;
  col : int;
}
type range = Ast.Pos.range = {
  start : pos;
  finish : pos;
}
let sexp_of_pos = Ast.Pos.sexp_of_t
let pos_of_sexp = Ast.Pos.t_of_sexp
let compare_pos = Ast.Pos.compare

let sexp_of_range = Ast.Pos.sexp_of_range
let range_of_sexp = Ast.Pos.range_of_sexp
let compare_range = Ast.Pos.compare_range

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
  (* Stdio.printf "----Parsed %s\n" (Ast.sexp_of_e parsed |> Sexp.to_string_hum); *)
  let (_, ir1) = Ir1.translate parsed in
  (* Stdio.printf "----IR1 %s\n" (Ir1.sexp_of_e ir1 |> Sexp.to_string_hum); *)
  Compiler.compile ~prelude ~comments ~repository ir1

let add_module ~repository ~path code =
  let open Result.Let_syntax in
  let%map (parsed, comments) = Syntax.parse_module ~path code in
  let (_, ir1) = Ir1.translate parsed in
  Compiler.add_module ~comments ~repository ~path ir1

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

module Debugger = struct
  module Var = Inter.Var

  include Inter.D
  type op = Inter.op
  type state = Inter.state
  type stack = Inter.stack
  type thread = Inter.thread = { id : int;
                                 op : op;
                                 env : (Var.t * Value.t) array;
                                 stack : stack;
                                 pos : pos}

  let init = Inter.init
  let tick = Inter.debug_tick
end
