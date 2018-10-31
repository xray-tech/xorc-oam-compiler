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
    with Not_found_s _ ->
      (* If the state number cannot be found -- which, in principle,
         should not happen, since our list of erroneous states is
         supposed to be complete! -- produce a generic message. *)
      Printf.sprintf "This is an unknown syntax error (%d).\n\
                      Please report this problem to the compiler vendor.\n" s

  in
  message

type comments = (string * Ast.Pos.range) list [@@deriving sexp_of]

let parse_with_error checkpoint lexbuf =
  with_return (fun r ->
      let comments = ref [] in
      let on_comment comment pos = comments := (comment, pos)::!comments in
      let supplier () =
        match Lexer.token on_comment lexbuf with
        | Ok(res) -> res
        | Error((pos, err)) ->
          r.return (Error(`SyntaxError(Ast.Pos.of_lexing pos, err))) in
      let module I = Parser.MenhirInterpreter in
      let succeed res = Ok (res, !comments)
      and fail checkpoint =
        let pos = lexbuf.Lexer.pos in
        Error(`SyntaxError(Ast.Pos.of_lexing pos, report checkpoint))
      in
      I.loop_handle succeed fail supplier checkpoint)

let parse_prog lexbuf =
  parse_with_error (Parser.Incremental.prog lexbuf.Lexer.pos) lexbuf
  |> Result.bind ~f:(function
      | (Some(v), comments) -> Ok(v, comments)
      | (None, _) -> Error(`NoInput))

let parse s =
  Lexer.create_lexbuf (Sedlexing.Utf8.from_string s)
  |> parse_prog

(* We implicity convert list of declaration to the normal AST with stop as final
   node *)
let parse_module ~path s =
  let open Result.Let_syntax in
  let fold_decls decl e =
    (Ast.EDecl(decl, e), Ast.Pos.dummy_range) in
  let lexbuf = Lexer.create_lexbuf ~path (Sedlexing.Utf8.from_string s) in
  let%map (res, comments) = parse_with_error (Parser.Incremental.orc_module lexbuf.Lexer.pos) lexbuf in
  (List.fold_right res ~init:(Ast.EModule, Ast.Pos.dummy_range) ~f:fold_decls, comments)

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
  |> Result.bind ~f:(fun (res, _comments) -> value_from_ast res)
