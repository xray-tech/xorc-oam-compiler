open Base

type parse_error =
  [ `NoInput
  | `SyntaxError of string * int * int * string]
[@@deriving sexp_of]

type parse_value_error =
  [ parse_error
  | `UnsupportedValueAST ]

type no_deps_error =
  [ `UnexpectedDependencies of string list ]
[@@deriving sexp_of]

type compile_error =
  [ `UnboundVar of string * Ast.pos
  | `UnknownReferedFunction of string * string ]
[@@deriving sexp_of]

type inter_error =
  [ `UnknownFFI of string ]
[@@deriving sexp_of]


let to_string_hum = function
  | `NoInput -> "No input to parse"
  | `SyntaxError(file, line, col, descr) -> Printf.sprintf "Syntax error in namespace %s at line %i column %i: %s" file (line + 1) col descr
  | `UnsupportedValueAST -> "Can't parse it as value"
  | `UnexpectedDependencies d -> Printf.sprintf "External dependencies are not supported here (%s)" (String.concat ~sep:", " d)
  | `UnboundVar(bind, {Ast.pstart}) ->
    Printf.sprintf "Unbound variable %s in file %s at line %i column %i" bind pstart.pos_fname (pstart.pos_lnum + 1) (pstart.pos_cnum - pstart.pos_bol + 1)
  | `BadFormat -> "Binary message is bad formatted"
  | `UnknownFFI def -> Printf.sprintf "Unsupported FFI call %s" def
  | `UnknownReferedFunction(ns, ident) -> Printf.sprintf "Unknown refered function %s:%s" ns ident


let foo () = Error(`Foo)

let bar () : (string, [> `Bar]) Result.t = Error(`Bar)

let foobar () =
  let open Result.Let_syntax in
  let%bind () = foo () in
  bar ()
