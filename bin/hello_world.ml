open Base
module Lexer = Orcml_lexer

let print_position outx lexbuf =
  let pos = lexbuf.Lexer.pos in
  Stdio.Out_channel.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let lexer () =
    let open Lexer in
    let ante_position = lexbuf.pos in
    let token = Lexer.token lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Orcml.Parser.prog in
  try parser lexer with
  | Orcml.Parser.Error ->
    Stdio.eprintf "%a: syntax error\n" print_position lexbuf;

    Stdio.print_endline "syntax error";
    None

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some(x) ->
    Stdio.print_endline (Sexp.to_string_hum (Orcml.Ast.sexp_of_e x))
  | None -> ()

let () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_channel Stdio.stdin) in
  parse_and_print lexbuf
