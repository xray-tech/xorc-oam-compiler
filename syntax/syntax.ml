open! Base
module Lexer = Orcml_lexer
module Errors = Orcml.Errors

let parse_with_error lexbuf =
  let lexer () =
    let open Lexer in
    let ante_position = lexbuf.pos in
    let token = Lexer.token lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Orcml.Parser.prog in
  Result.try_with (fun () -> parser lexer)
  |> Result.map_error ~f:(fun _ ->
      let pos = lexbuf.Lexer.pos in
      Errors.SyntaxError { filename = pos.pos_fname;
                           line = pos.pos_lnum;
                           col = (pos.pos_cnum - pos.pos_bol + 1)})
  |> Result.bind ~f:(function
      | Some(v) -> Ok(v)
      | None -> Error(Errors.NoInput))

let from_string s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_with_error

let value s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_with_error
  |> function
  | Error _ -> None
  | Ok(Orcml.Ast.EConst const, _) -> Some(Orcml.Inter.VConst(const))
  | _ -> None
