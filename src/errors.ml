open Base

type parse_error =
  [ `NoInput
  | `SyntaxError of (int * int) * string]
[@@deriving sexp_of]

type parse_value_error =
  [ parse_error
  | `UnsupportedValueAST ]

type compile_error =
  [ `UnboundVar of (int * int) * string
  | `UnknownReferedFunction of string * string ]
[@@deriving sexp_of]

type inter_error =
  [ `UnknownFFI of string ]
[@@deriving sexp_of]

let to_string_hum = function
  | `NoInput -> "No input to parse"
  | `SyntaxError((line, col), descr) -> Printf.sprintf "Syntax error at line %i column %i: %s" line col descr
  | `UnsupportedValueAST -> "Can't parse it as value"
  | `UnexpectedDependencies d -> Printf.sprintf "External dependencies are not supported here (%s)" (String.concat ~sep:", " d)
  | `UnboundVar((line, col), bind) ->
    Printf.sprintf "Unbound variable %s at line %i column %i" bind line col
  | `BadFormat -> "Binary message is bad formatted"
  | `UnknownFFI def -> Printf.sprintf "Unsupported FFI call %s" def
  | `UnknownReferedFunction(mod_, ident) -> Printf.sprintf "Unknown refered function %s:%s" mod_ ident
