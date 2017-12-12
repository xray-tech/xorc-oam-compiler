type ast = Ast.e
let sexp_of_ast = Ast.sexp_of_e
let compare_ast = Ast.compare_e

type env = Inter.env
module Const = struct
  type t = Ast.const
  let compare = Ast.compare_const
  let sexp_of_t = Ast.sexp_of_const
end


module Value = struct
  type t = Inter.v
  let sexp_of_t = Inter.sexp_of_v
  let t_of_sexp = Inter.v_of_sexp
  let compare = Inter.compare_v

  include Inter.Value
end

type ir1 = Ir1.e
let sexp_of_ir1 = Ir1.sexp_of_e

let parse = Syntax.parse

let parse_ns = Syntax.parse_ns

let parse_value = Syntax.parse_value

let translate = Ir1.translate

let translate_no_deps = Ir1.translate_no_deps

module Fun = Compiler.Fun

type repo = Compiler.repo
let sexp_of_repo = Compiler.sexp_of_repo
let compare_repo = Compiler.compare_repo

type imports = Compiler.imports
type linker = Compiler.linker

let link = Compiler.link

let compile = Compiler.compile
let compile_ns = Compiler.compile_ns

type bc = Inter.bc
let sexp_of_bc = Inter.sexp_of_bc
let compare_bc = Inter.compare_bc

let finalize = Compiler.finalize

type coeffect = Inter.coeffect
type instance = Inter.instance

module Res = Inter.Res

let run = Inter.run
let unblock = Inter.unblock

module Serializer = Serializer

module Testkit = Testkit
