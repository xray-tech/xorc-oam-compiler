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

let value s =
  let lexbuf = Orcml_lexer.create_lexbuf (Sedlexing.Utf8.from_string s) in
  let module A = Orcml.Ast in
  parse_with_error lexbuf
  |> Option.bind ~f:(function
      | (A.EConst(v), _) -> Some(Orcml.Inter.VConst(v))
      | _ -> None)

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
    (match Orcml.Ir1.translate x with
    | Ok(res) ->
      Stdio.print_endline (Sexp.to_string_hum (Orcml.Ir1.sexp_of_e res))
    | Error() -> Stdio.printf "Error")
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
    let (values, _, _, _) = Orcml.Inter.run res in
    List.iter values
      (fun v ->
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

let print_result (values, coeffects, _, snapshot) =
  List.iter values
    (fun v ->
       Stdio.eprintf "%s\n" (Sexp.to_string_hum (Orcml.Inter.sexp_of_v v)));
  List.iter coeffects
    (fun (i, v) ->
       Stdio.eprintf "Coeffect %i -> %s\n" i (Sexp.to_string_hum (Orcml.Inter.sexp_of_v v)));
  Stdio.printf "%s" (Orcml.Serialize.serialize snapshot)


let byterun () =
  let input = Stdio.In_channel.input_all Stdio.stdin in
  let prog = Orcml.Serialize.deserialize_bc input in
  print_result (Orcml.Inter.run prog)

let unblock prog_path id v =
  let module Serialize = Orcml.Serialize in
  let snapshot = Serialize.deserialize (Stdio.In_channel.input_all Stdio.stdin) in
  let prog = Serialize.deserialize_bc (Stdio.In_channel.read_all prog_path) in
  let id' = Int.of_string id in
  let v' = Option.value_exn (value v) in
  print_result (Orcml.Inter.unblock prog snapshot id' v')

let () =
  let argv = Caml.Sys.argv in
  match argv.(1) with
  | "bench" -> bench ()
  | "parse" -> parse_and_print ()
  | "ir1" -> ir1 ()
  | "compile" -> compile ()
  | "bytegen" -> bytegen ()
  | "byterun" -> byterun ()
  | "unblock" -> unblock argv.(2) argv.(3) argv.(4)
  | "eval" -> eval ()
  | _ -> Caml.exit(-1)
