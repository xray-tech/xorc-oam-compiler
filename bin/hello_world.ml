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

let parse_and_print () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_channel Stdio.stdin) in
  match parse_with_error lexbuf with
  | Some(x) ->
    Stdio.print_endline (Sexp.to_string_hum (Orcml.Ast.sexp_of_e x))
  | None -> ()

let parse_and_ignore s =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_string s) in
  parse_with_error lexbuf |> ignore

let bench () =
  let s = Stdio.In_channel.input_all Stdio.stdin in
  let samples = Benchmark.latency1 1000L parse_and_ignore s in
  Benchmark.tabulate samples

let ir1 () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_channel Stdio.stdin) in
  match parse_with_error lexbuf with
  | Some(x) ->
    let res = Orcml.Ir1.translate x in
    Stdio.print_endline (Sexp.to_string_hum (Orcml.Ir1.sexp_of_e res))
  | None -> ()


let compile () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_channel Stdio.stdin) in
  match parse_with_error lexbuf with
  | Some(x) ->
    let ir1 = Orcml.Ir1.translate x in
    let res = Orcml.Compiler.compile ir1 in
    Stdio.print_endline (Sexp.to_string_hum (Orcml.Inter.sexp_of_code res))
  | None -> ()

let eval () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_channel Stdio.stdin) in
  match parse_with_error lexbuf with
  | Some(x) ->
    let ir1 = Orcml.Ir1.translate x in
    let res = Orcml.Compiler.compile ir1 in
    Orcml.Inter.run res (fun v ->
        Stdio.print_endline (Sexp.to_string_hum (Orcml.Inter.sexp_of_v v)))
  | None -> ()

let bytegen () =
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_channel Stdio.stdin) in
  match parse_with_error lexbuf with
  | Some(x) ->
    let ir1 = Orcml.Ir1.translate x in
    let compiled = Orcml.Compiler.compile ir1 in
    Stdio.printf "%s" (Orcml.Serialize.serialize_bc compiled)
  | None -> ()

let byterun () =
  let input = Stdio.In_channel.input_all Stdio.stdin in
  let prog = Orcml.Serialize.deserialize_bc input in
  Orcml.Inter.run prog (fun v ->
      Stdio.print_endline (Sexp.to_string_hum (Orcml.Inter.sexp_of_v v)))

let () =
  match Caml.Sys.argv.(1) with
  | "bench" -> bench ()
  | "parse" -> parse_and_print ()
  | "ir1" -> ir1 ()
  | "compile" -> compile ()
  | "bytegen" -> bytegen ()
  | "byterun" -> byterun ()
  | "eval" -> eval ()
  | _ -> Caml.exit(-1)
