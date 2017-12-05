open! Base
module Lexer = Orcml_lexer
module Errors = Orcml.Errors

let parse_with_error p lexbuf =
  let lexer () =
    let open Lexer in
    let ante_position = lexbuf.pos in
    let token = Lexer.token lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser = MenhirLib.Convert.Simplified.traditional2revised p in
  Result.try_with (fun () -> parser lexer)
  |> Result.map_error ~f:(fun _ ->
      let pos = lexbuf.Lexer.pos in
      Errors.create (Errors.SyntaxError { filename = pos.pos_fname;
                                       line = pos.pos_lnum;
                                       col = (pos.pos_cnum - pos.pos_bol + 1)}))

let parse_prog lexbuf =
  parse_with_error Orcml.Parser.prog lexbuf
  |> Result.bind ~f:(function
      | Some(v) -> Ok(v)
      | None -> Errors.err Errors.NoInput)

let from_string s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_prog

let ns_from_string s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_with_error Orcml.Parser.ns

let rec value_from_ast (e, _) =
  let rec list acc = function
    | [] -> Some(List.rev acc)
    | x::xs -> Option.bind (value_from_ast x) (fun v ->
        list (v::acc) xs) in
  let rec pairs acc = function
    | [] -> Some(List.rev acc)
    | (f, x)::xs -> Option.bind (value_from_ast x) (fun v ->
        pairs((f, v)::acc) xs) in
  match e with
  | Orcml.Ast.EConst const -> Some(Orcml.Inter.VConst(const))
  | Orcml.Ast.ETuple vs ->
    Option.map (list [] vs) (fun l -> Orcml.Inter.VTuple l)
  | Orcml.Ast.EList vs ->
    Option.map (list [] vs) (fun l -> Orcml.Inter.VList l)
  | Orcml.Ast.ERecord vs ->
    Option.map (pairs [] vs) (fun l -> Orcml.Inter.VRecord l)
  | _ -> None

let value s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_prog
  |> function
  | Error _ -> None
  | Ok(ast) -> value_from_ast ast
