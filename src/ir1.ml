open Base
type e' =
  | EOtherwise of e * e
  | EParallel of e * e
  | EPruning of e * string option * e
  | ESequential of e * string option * e
  | EConst of Ast.const
  | EIdent of string
  | ECall of string * string list
  | EStop
  | ESite of string * string * e
  | EFix of (string * string list * e) list * e
and e = e' * Ast.e [@@deriving sexp_of]

let fresh = ref 0

let make_fresh () =
  let i = !fresh in
  fresh := i + 1;
  "'fresh" ^ (Int.to_string i)

let rec collect_defs ((ident, _, _, _) as d) e =
  let rec collect_clauses current (x::acc as res) = function
    | (Ast.EDecl((DDef(ident, params, guard, body), _), e), _) when String.equal ident current ->
      collect_clauses current (((ident, params, guard, body)::x)::acc) e
    | (Ast.EDecl((DDef(ident, params, guard, body), _), e), _) ->
      collect_clauses ident ([(ident, params, guard, body)]::res) e
    | e -> (res, e) in
  collect_clauses ident [[d]] e

(* TODO *)
let divide_defs defs =
  [defs]

let rec translate' ((e, pos) as ast) =
  let module A = Ast in
  (* Sexp.to_string_hum (Ast.sexp_of_e' e ) |> Stdio.print_endline; *)
  match e with
  | A.EIdent(v) -> (EIdent v, ast)
  | A.EOtherwise(e1, e2) ->
    (EOtherwise(translate' e1, translate' e2), ast)
  | A.EParallel(e1, e2) ->
    (EParallel(translate' e1, translate' e2), ast)
  | A.EPruning(e1, ((A.PVar i), _), e2) ->
    (EPruning(translate' e1, Some i, translate' e2), ast)
  | A.EPruning(e1, ((A.PWildcard), _), e2) ->
    (EPruning(translate' e1, None, translate' e2), ast)
  | A.EPruning(e1, (p, _), e2) ->
    raise Util.TODO
  | A.ESequential(e1, ((A.PVar i), _), e2) ->
    (ESequential(translate' e1, Some i, translate' e2), ast)
  | A.ESequential(e1, ((A.PWildcard), _), e2) ->
    (ESequential(translate' e1, None, translate' e2), ast)
  | A.ESequential(e1, (p, _), e2) ->
    raise Util.TODO
  | A.EList(es) ->
    deflate_many es (fun es' ->
        (ECall("'MakeList", es'), ast))
  | A.ETuple(es) ->
    deflate_many es (fun es' ->
        (ECall("'MakeTuple", es'), ast))
  | A.ERecord(pairs) ->
    let (keys, vals) = List.unzip pairs in
    let key_consts = (List.map keys (fun k ->
        (A.EConst(A.String k), A.dummy))) in
    deflate_many key_consts (fun keys' ->
        deflate_many vals (fun vals' ->
            let args = List.fold2_exn keys' vals' ~init:[] ~f:(fun acc k v ->
                k::v::acc) |> List.rev in
            (ECall("'MakeRecord", args), ast) ))
  | A.ECond(pred, then_, else_) ->
    deflate pred (fun pred' ->
        (EParallel((ESequential((ECall("'Ift", [pred']), ast), None, translate' then_), ast),
                   (ESequential((ECall("'Iff", [pred']), ast), None, translate' else_), ast)),
        ast))
  | A.EConst c -> (EConst(c), ast)
  | A.ECall(e, args) ->
    deflate e (fun e' ->
        deflate_many args (fun args' ->
            (ECall(e', args'), ast)))
  | A.EStop -> (EStop, ast)
  | A.ELambda(params, body) ->
    let n = make_fresh () in
    let e' = A.EDecl((DDef(n, params, None, body), A.dummy),
                     (A.EIdent n, A.dummy)) in
    translate'((e', A.dummy))
  | A.EDecl((DDef(ident, params, guard, body), _), e) ->
    translate_defs (ident, params, guard, body) e
  | A.EDecl((DVal(p, val_e), _), e) ->
    translate' ((EPruning(e, p, val_e)), pos)
  | A.EFieldAccess(target, field) ->
    deflate target (fun t ->
        (ECall("'FieldAccess", [t]), ast))
  | A.EDecl((DSite(ident, definition), _), e) ->
    raise Util.TODO
  | A.EDecl((DData(ident, constructors), _), e) ->
    raise Util.TODO
  | A.EDecl((DInclude(_), _), e) ->
    assert false


and deflate e k =
  match e with
  | (Ast.EIdent(v), _) -> k v
  | ast ->  let f = make_fresh () in
    (EPruning(k f, Some f, translate' e), ast)
and deflate_many es k =
  let rec step acc = function
    | e::es ->
      deflate e (fun i ->
          step (i::acc) es)
    | [] -> k (List.rev acc) in
  step [] es
and translate_defs d e =
  let (defs, e') = collect_defs d e in
  let defs' = translate_clauses defs in
  let def_groups = divide_defs defs' in
  let rec step = function
    | group::xs -> (EFix(group, step xs), e)
    | [] -> translate' e' in
  step def_groups
and translate_clauses defs =
  List.map defs (function
      | [(ident, params, guard, body)] ->
        let params' = List.map params (function
            | (PVar n, _) -> n
            | _ -> raise Util.TODO) in
        (ident, params', translate' body)
      | _ -> raise Util.TODO )

let translate e = Errors.try_with (fun () -> translate' e)
