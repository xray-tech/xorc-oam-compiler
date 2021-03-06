open Base

type parse_error =
  [ `NoInput
  | `SyntaxError of Ast.Pos.t * string]
[@@deriving sexp_of]

type parse_value_error =
  [ parse_error
  | `UnsupportedValueAST ]

type compile_error =
  [ `UnboundVar of Ast.Pos.range * string
  | `UnknownReferedFunction of string * string ]
[@@deriving sexp_of]

type inter_error =
  [ `UnknownFFC of string ]
[@@deriving sexp_of]

let to_string_hum = function
  | `NoInput -> "No input to parse"
  | `SyntaxError({Ast.Pos.line; col}, descr) -> Printf.sprintf "Syntax error at line %i column %i: %s" line col descr
  | `UnsupportedValueAST -> "Can't parse it as value"
  | `UnexpectedDependencies d -> Printf.sprintf "External dependencies are not supported here (%s)" (String.concat ~sep:", " d)
  | `UnboundVar({ Ast.Pos.start = {line; col} }, bind) ->
    Printf.sprintf "Unbound variable %s at line %i column %i" bind line col
  | `BadFormat -> "Binary message is bad formatted"
  | `UnknownFFC def -> Printf.sprintf "Unsupported FFC call %s" def
  | `UnknownReferedFunction(mod_, ident) -> Printf.sprintf "Unknown refered function %s:%s" mod_ ident
