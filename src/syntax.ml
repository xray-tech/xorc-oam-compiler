open! Common

let report checkpoint =
  let module S = MenhirLib.General in
  let module Inter = Parser.MenhirInterpreter in
  let stack checkpoint =
    match checkpoint with
    | Inter.HandlingError env ->
      Inter.stack env
    | _ ->
      assert false in
  let state checkpoint =
    match Lazy.force (stack checkpoint) with
    | S.Nil ->
      0
    | S.Cons (Inter.Element (s, _, _, _), _) ->
      Inter.number s in
  let s = state checkpoint in
  (* Choose an error message, based on the state number [s].
     Then, customize it, based on dynamic information. *)
  let message = try
      Printf.sprintf "%s (error code %d)" (Parser_messages.message s) s
    with Not_found ->
      (* If the state number cannot be found -- which, in principle,
         should not happen, since our list of erroneous states is
         supposed to be complete! -- produce a generic message. *)
      Printf.sprintf "This is an unknown syntax error (%d).\n\
                      Please report this problem to the compiler vendor.\n" s
  in
  message

let parse_with_error p checkpoint fname lexbuf =
  let supplier () =
    let open Lexer in
    let ante_position = lexbuf.pos in
    let token = Lexer.token lexbuf in
    let post_position = lexbuf.pos
    in (token, ante_position, post_position) in
  let module I = Parser.MenhirInterpreter in
  let succeed res = Ok(res)
  and fail checkpoint =
    let pos = lexbuf.Lexer.pos in
    Error(`SyntaxError(fname, pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1), report checkpoint))
  in
  I.loop_handle succeed fail supplier checkpoint

let parse_prog lexbuf =
  parse_with_error Parser.prog (Parser.Incremental.prog lexbuf.Lexer.pos) "" lexbuf
  |> Result.bind ~f:(function
      | Some(v) -> Ok(v)
      | None -> Error(`NoInput))

let parse s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_prog

(* We implicity convert list of declaration to the normal AST with stop as final
   node *)
let parse_ns ~filename s =
  let open Result.Let_syntax in
  let fold_decls decl e =
    (Ast.EDecl(decl, e), Ast.dummy) in
  let lexbuf = Lexer.create_lexbuf (Sedlexing.Utf8.from_string s) in
  let%map res = parse_with_error Parser.ns (Parser.Incremental.ns lexbuf.Lexer.pos) filename lexbuf in
  List.fold_right res ~init:(Ast.ENS, Ast.dummy) ~f:fold_decls

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
        | _ -> Error(`UnsupportedValueAST) |> r.return in
      Ok(f e))


let parse_value s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_prog
  |> Result.bind ~f:value_from_ast
