open Core
open Lexing

module Parser = Orcml.Parser
module Lexer = Orcml.Lexer

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some e ->
    print_endline (Sexp.to_string_hum (Orcml.Ast.sexp_of_e e));
    parse_and_print lexbuf
  | None -> ()

let () =
  let lexbuf = Lexing.from_string "def foo(a) = a + 2 foo(3)" in
  parse_and_print lexbuf
