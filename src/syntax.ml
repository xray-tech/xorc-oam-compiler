open! Common

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
  parse_with_error Parser.prog lexbuf
  |> Result.bind ~f:(function
      | Some(v) -> Ok(v)
      | None -> Errors.err Errors.NoInput)

let parse s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_prog

(* We implicity convert list of declaration to the normal AST with stop as final
   node *)
let parse_ns s =
  let open Result.Let_syntax in
  let fold_decls decl e =
    (Ast.EDecl(decl, e), Ast.dummy) in
  let%map res = Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
                |> parse_with_error Parser.ns in
  List.fold_right res ~init:(Ast.EStop, Ast.dummy) ~f:fold_decls

let value_from_ast e =
  with_return (fun r ->
      let rec f (e, _) =
        match e with
        | Ast.EConst const -> Inter.VConst(const)
        | Ast.ETuple vs ->
          Inter.VTuple (List.map vs ~f)
        | Ast.EList vs ->
          Inter.VList (List.map vs ~f)
        | Ast.ERecord vs ->
          Inter.VRecord (List.Assoc.map vs ~f)
        | _ -> Or_error.errorf "Incompatible AST" |> r.return in
      Ok(f e))


let parse_value s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_prog
  |> Result.bind ~f:value_from_ast
