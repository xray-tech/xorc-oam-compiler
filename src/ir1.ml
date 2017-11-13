open Base
type e =
  | EOtherwise of e * e
  | EParallel of e * e
  | EPruning of e * string option * e
  | ESequential of e * string option * e
  | EConst of Ast.const
  | EIdent of string
  | ECall of string * string list
  | EStop
  | ESite of string * string * e
  | EFix of (string * string list * e) list * e [@@deriving variants, sexp]

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

let rec translate (e, pos) =
  let module A = Ast in
  (* Sexp.to_string_hum (Ast.sexp_of_e' e ) |> Stdio.print_endline; *)
  match e with
  | A.EIdent(v) -> eident v
  | A.EOtherwise(e1, e2) ->
    eotherwise (translate e1) (translate e2)
  | A.EParallel(e1, e2) ->
    eparallel (translate e1) (translate e2)
  | A.EPruning(e1, ((A.PVar i), _), e2) ->
    epruning (translate e1) (Some i) (translate e2)
  | A.EPruning(e1, ((A.PWildcard), _), e2) ->
    epruning (translate e1) None (translate e2)
  | A.EPruning(e1, (p, _), e2) ->
    raise Util.TODO
  | A.ESequential(e1, ((A.PVar i), _), e2) ->
    esequential (translate e1) (Some i) (translate e2)
  | A.ESequential(e1, ((A.PWildcard), _), e2) ->
    esequential (translate e1) None (translate e2)
  | A.ESequential(e1, (p, _), e2) ->
    raise Util.TODO
  | A.EList(es) ->
    deflate_many es (fun es' ->
        ecall "'MakeList" es')
  | A.ETuple(es) ->
    deflate_many es (fun es' ->
        ecall "'MakeTuple" es')
  | A.ERecord(pairs) ->
    let (keys, vals) = List.unzip pairs in
    let key_consts = (List.map keys (fun k ->
        (A.EConst(A.String k), A.dummy))) in
    deflate_many key_consts (fun keys' ->
        deflate_many vals (fun vals' ->
            let args = List.fold2_exn keys' vals' ~init:[] ~f:(fun acc k v ->
                k::v::acc) |> List.rev in
            ecall "'MakeRecord" args ))
  | A.ECond(pred, then_, else_) ->
    deflate pred (fun pred' ->
        EParallel(esequential (ecall "'Ift" [pred']) None (translate then_),
                  esequential (ecall "'Iff" [pred']) None (translate else_)))
  | A.EConst c -> econst c
  | A.ECall(e, args) ->
    deflate e (fun e' ->
        deflate_many args (fun args' ->
            ecall e' args'))
  | A.EStop -> EStop
  | A.ELambda(params, body) ->
    let n = make_fresh () in
    let e' = A.EDecl((DDef(n, params, None, body), A.dummy),
                     (A.EIdent n, A.dummy)) in
    translate((e', A.dummy))
  | A.EDecl((DDef(ident, params, guard, body), _), e) ->
    translate_defs (ident, params, guard, body) e
  | A.EDecl((DVal(p, val_e), _), e) ->
    translate ((EPruning(e, p, val_e)), pos)
  | A.EFieldAccess(target, field) ->
    deflate target (fun t ->
        ecall "'FieldAccess" [t])
  | A.EDecl((DSite(ident, definition), _), e) ->
    raise Util.TODO
  | A.EDecl((DData(ident, constructors), _), e) ->
    raise Util.TODO
  | A.EDecl((DInclude(_), _), e) ->
    assert false


and deflate e k =
  match e with
  | (Ast.EIdent(v), _) -> k v
  | _ ->  let f = make_fresh () in
    epruning (k f) (Some f) (translate e)
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
    | group::xs -> efix group (step xs)
    | [] -> translate e' in
  step def_groups
and translate_clauses defs =
  List.map defs (function
      | [(ident, params, guard, body)] ->
        let params' = List.map params (function
            | (PVar n, _) -> n
            | _ -> raise Util.TODO) in
        (ident, params', translate body)
      | _ -> raise Util.TODO )
