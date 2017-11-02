module Lexer = Orcml_lexer

open Base
let parse_with_error lexbuf =
  let lexer () =
    let open Lexer in
    let ante_position = lexbuf.pos in
    let token = Lexer.token lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Orcml.Parser.prog in
  try parser lexer with
  | _ ->
    Stdio.print_endline "syntax error";
    None

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some(x) ->
    Stdio.print_endline (Sexp.to_string_hum (Orcml.Ast.sexp_of_e x))
  | None -> ()

let () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_string "1") in
  parse_and_print lexbuf
