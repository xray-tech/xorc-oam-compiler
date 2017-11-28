open Base
open Sexplib
open Conv

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
and e = e' * Ast.e sexp_opaque [@@deriving sexp_of]

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
  let module DSL = struct
    let const c =
      (EConst c, ast)

    let par left right =
      (EParallel(left, right), ast)

    let seqw left right =
      (ESequential(left, None, right), ast)

    let seq left bind right =
      (ESequential(left, Some(bind), right), ast)

    let prunew left right = (EPruning(left, None, right), ast)

    let prune left bind right =
      (EPruning(left, Some(bind), right), ast)

    let call target params =
      (ECall(target, params), ast)

    let ident v =
      (EIdent(v), ast)

    let deflate' e k =
      match e with
      | EIdent(v), _ -> k v
      | ast -> let f = make_fresh () in
        prune (k f) f e

    let deflate e k = deflate' (translate' e) k end in
  let module A = Ast in
  let translate_pattern p source =
    let module A = Ast in
    let bindings = ref [] in
    let rec unravel (p, pos) focus expr =
      match p with
      | A.PAs(p, alias) ->
        bindings := (focus, alias)::!bindings;
        unravel p focus expr
      | A.PVar(x) ->
        bindings := (focus, x)::!bindings;
        expr ()
      | A.PWildcard -> expr ()
      | A.PConst(x) ->
        DSL.(deflate' (const x) (fun x' ->
            deflate' (call "=" [focus; x']) (fun is_eq ->
                seqw
                  (call "'Ift" [is_eq])
                  (expr ()))))
      | A.PTuple(ps) ->
        DSL.(deflate' (const (A.Int (List.length ps))) (fun arity ->
            seqw (call "'ArityCheck" [focus; arity])
              (unravel_tuple ps 0 focus expr)))
      | A.PCons(head, tail) ->
        let head' = make_fresh () in
        let tail' = make_fresh () in
        DSL.(seq
               (call "'First" [focus]) head'
               (seq
                  (call "'Rest" [focus]) tail'
                  (unravel head head' (fun () ->
                       unravel tail tail' expr))))
      | A.PList(ps) ->
        DSL.(deflate' (const (A.Int (List.length ps))) (fun size ->
            seqw (call "'ListSizeCheck" [focus; size])
              (unravel_list ps focus expr)))
      | A.PRecord(pairs) ->
        unravel_record pairs focus expr
    and unravel_record pairs focus expr =
      match pairs with
      | [] -> expr ()
      | (f, p)::pairs' ->
        let bind = make_fresh () in
        DSL.(deflate' (const (A.String f)) (fun field ->
            seq
              (call "'FieldAccess" [focus; field])
              bind
              (unravel p bind (fun () ->
                   unravel_record pairs' focus expr))))
    and unravel_list ps focus expr =
      match ps with
      | [] -> expr ()
      | p::ps' ->
        let head' = make_fresh () in
        let tail' = make_fresh () in
        DSL.(seq
               (call "'First" [focus]) head'
               (seq
                  (call "'Rest" [focus]) tail'
                  (unravel p head' (fun () ->
                       unravel_list ps' tail' expr))))
    and unravel_tuple ps i focus expr =
      match ps with
      | [] -> expr ()
      | p::ps' ->
        DSL.(deflate' (const (A.Int i)) (fun i' ->
            let bind = make_fresh () in
            seq (call focus [i']) bind (unravel p bind (fun () ->
                unravel_tuple ps' (i + 1) focus expr)))) in
    let on_source () = if List.length !bindings > 0
      then DSL.call "'MakeTuple" (List.map !bindings (fun (b, _) -> b))
      else DSL.const A.Signal in
    let on_target bridge target =
      if List.length !bindings > 0
      then let (_, res) = List.fold !bindings ~init:(0, target) ~f:(fun (i, target') (_, binding) ->
          (i + 1,
           DSL.(deflate' (const (A.Int i)) (fun i' ->
               seq (call bridge [i']) binding target')))) in
        res
      else target in
    (unravel p source on_source,
     on_target) in
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
  | A.EPruning(e1, p, e2) ->
    let source_binding = make_fresh () in
    let bridge = make_fresh () in
    let (source, on_target) = translate_pattern p source_binding in
    DSL.(prune
           (on_target bridge (translate' e1))
           bridge
           (seq (translate' e2) source_binding source))
  | A.ESequential(e1, ((A.PVar i), _), e2) ->
    (ESequential(translate' e1, Some i, translate' e2), ast)
  | A.ESequential(e1, ((A.PWildcard), _), e2) ->
    (ESequential(translate' e1, None, translate' e2), ast)
  | A.ESequential(e1, p, e2) ->
    let source_binding = make_fresh () in
    let bridge = make_fresh () in
    let (source, on_target) = translate_pattern p source_binding in
    DSL.(seq
           (translate' e1)
           source_binding
           (seq source bridge (on_target bridge (translate' e2))))
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
                k::v::acc) in
            (ECall("'MakeRecord", args), ast) ))
  | A.ECond(pred, then_, else_) ->
    DSL.(
      deflate pred (fun pred' ->
          par
            (seqw (call "'Ift" [pred']) (translate' then_))
            (seqw (call "'Iff" [pred']) (translate' else_))))
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
        deflate (A.EConst(A.String field), A.dummy) (fun f ->
            (ECall("'FieldAccess", [t; f]), ast)))
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
