open Base
open Sexplib
open Conv

module Var = struct
  type t = | Generated of string
           | Handcrafted of { ident : string;
                              pos : Ast.Pos.range }
  [@@deriving sexp, compare]

  let to_indexed index = function
    | Generated _ ->
      Inter0.Var.Generated index
    | Handcrafted { ident; pos } ->
      Inter0.Var.Handcrafted { ident; pos; index }

  let ident = function
    | Generated ident -> ident
    | Handcrafted { ident } -> ident

  let idents = List.map ~f:ident

end


type e' =
  | EOtherwise of e * e
  | EParallel of e * e
  | EPruning of e * Var.t option * e
  | ESequential of e * Var.t option * e
  | EConst of Ast.const
  | EIdent of string
  | ECall of string * string list
  | EFFC of string * string list
  | EStop
  | EFix of (string * Var.t list * e) list * e
  | ERefer of string * string list * e
  | EModule
and e = e' * Ast.e [@@deriving sexp_of]

let fresh = ref 0
let deps = ref []

let make_fresh () =
  let i = !fresh in
  fresh := i + 1;
  "'fresh" ^ (Int.to_string i)

let collect_defs e =
  let acc = Hashtbl.create (module String) in
  let rec collect_clauses = function
    | (Ast.EDecl((DDef(ident, _, params, _, guard, body), _), e), _) ->
      let clauses = Hashtbl.find_or_add acc ident ~default:(fun () -> []) in
      Hashtbl.set acc ~key:ident ~data:((ident, params, guard, body)::clauses);
      collect_clauses e
    | (Ast.EDecl((DSig _, _), e), _) ->
      collect_clauses e
    | e -> (Hashtbl.data acc, e) in
  collect_clauses e

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

    (* let prunew left right = (EPruning(left, None, right), ast) *)

    let prune left bind right =
      (EPruning(left, Some(bind), right), ast)

    let call target params =
      (ECall(target, params), ast)

    let ffc target params =
      (EFFC(target, params), ast)

    let otherwise left right =
      (EOtherwise(left, right), ast)

    let ident v =
      (EIdent(v), ast)

    let stop = (EStop, ast)

    let deflate' e k =
      match e with
      | EIdent(v), _ -> k v
      | _ -> let f = make_fresh () in
        prune (k f) (Var.Generated f) e

    let deflate e k = deflate' (translate' e) k end in
  let module A = Ast in
  let rec translate_pattern p source =
    let module A = Ast in
    let bindings = ref [] in
    let rec unravel (p, range) focus expr =
      match p with
      | A.PAs(p, alias) ->
        bindings := (focus, alias, range)::!bindings;
        unravel p focus expr
      | A.PVar(x) ->
        bindings := (focus, x, range)::!bindings;
        expr ()
      | A.PWildcard -> expr ()
      | A.PConst(x) ->
        DSL.(deflate' (const x) (fun x' ->
            deflate' (call "=" [focus; x']) (fun is_eq ->
                seqw
                  (ffc "core.ift" [is_eq])
                  (expr ()))))
      | A.PTuple(ps) ->
        DSL.(deflate' (const (A.Int (List.length ps))) (fun arity ->
            seqw (ffc "core.arity-check" [focus; arity])
              (unravel_tuple ps 0 focus expr)))
      | A.PCons(head, tail) ->
        let head' = make_fresh () in
        let tail' = make_fresh () in
        DSL.(seq
               (ffc "core.first" [focus]) (Var.Generated head')
               (seq
                  (ffc "core.rest" [focus]) (Var.Generated tail')
                  (unravel head head' (fun () ->
                       unravel tail tail' expr))))
      | A.PList(ps) ->
        DSL.(deflate' (const (A.Int (List.length ps))) (fun size ->
            seqw (ffc "core.list-check-size" [focus; size])
              (unravel_list ps focus expr)))
      | A.PRecord(pairs) ->
        unravel_record pairs focus expr
      | A.PTyped(p, _) ->
        unravel p focus expr
      | A.PCall _ ->
        raise Util.TODO
    and unravel_record pairs focus expr =
      match pairs with
      | [] -> expr ()
      | (f, p)::pairs' ->
        let bind = make_fresh () in
        DSL.(deflate' (const (A.String f)) (fun field ->
            seq
              (ffc "core.field-access" [focus; field])
              (Var.Generated bind)
              (unravel p bind (fun () ->
                   unravel_record pairs' focus expr))))
    and unravel_list ps focus expr =
      match ps with
      | [] -> expr ()
      | p::ps' ->
        let head' = make_fresh () in
        let tail' = make_fresh () in
        DSL.(seq
               (ffc "core.first" [focus]) (Var.Generated head')
               (seq
                  (ffc "core.rest" [focus]) (Var.Generated tail')
                  (unravel p head' (fun () ->
                       unravel_list ps' tail' expr))))
    and unravel_tuple ps i focus expr =
      match ps with
      | [] -> expr ()
      | p::ps' ->
        DSL.(deflate' (const (A.Int i)) (fun i' ->
            let bind = make_fresh () in
            seq (call focus [i']) (Var.Generated bind) (unravel p bind (fun () ->
                unravel_tuple ps' (i + 1) focus expr)))) in
    let on_source () = if List.length !bindings > 0
      then DSL.ffc "core.make-tuple" (List.map !bindings ~f:(fun (b, _, _) -> b))
      else DSL.const A.Signal in
    let on_target bridge target =
      if List.length !bindings > 0
      then let (_, res) = List.fold !bindings ~init:(0, target) ~f:(fun (i, target') (_, binding, range) ->
          let var = Var.Handcrafted { ident = binding; pos = range } in
          (i + 1,
           DSL.(deflate' (const (A.Int i)) (fun i' ->
               seq (call bridge [i']) var target')))) in
        res
      else target in
    (unravel p source on_source,
     on_target)
  and translate_defs e =
    let (defs, e') = collect_defs e in
    let defs' = translate_clauses defs in
    let def_groups = divide_defs defs' in
    let rec step = function
      | group::xs -> (EFix(group, step xs), e)
      | [] -> translate' e' in
    step def_groups
  and make_match source target else_ =
    let res = make_fresh () in
    let maybe_res = make_fresh () in
    let target_binding = make_fresh () in
    DSL.(seq
           (otherwise
              (seq source (Var.Generated res) (ffc "core.wrap-some" [res]))
              (ffc "core.get-none" []))
           (Var.Generated maybe_res)
           (par
              (seq (ffc "core.unwrap-some" [maybe_res]) (Var.Generated target_binding) (target target_binding))
              (seqw (ffc "core.is-none" [maybe_res]) else_)))
  and translate_clauses defs =
    let rebind_vars vars expr =
      List.fold vars ~init:expr ~f:(fun acc (p, binding) ->
          match p with
          | (Ast.PVar(v), range) -> DSL.(seq (ident binding) (Var.Handcrafted { ident = v; pos = range}) acc)
          | _ -> assert false) in
    let clause' vars strict else_ guard body =
      let stricted_values = make_fresh () in
      let pattern = Ast.PTuple (List.map strict ~f:(fun (x, _) -> x)) in
      let values = DSL.ffc "core.make-tuple" (List.map strict ~f:(fun (_, x) -> x)) in
      let (source, on_target) = translate_pattern (pattern, pos) stricted_values in
      let source' = match guard with
        | Some(v) -> DSL.(let bind = make_fresh () in
                          let guarded = deflate v (fun res ->
                              (seqw (ffc "core.ift" [res]) (ident bind))) in
                          seq source (Var.Generated bind) (on_target bind (rebind_vars vars guarded)))
        | None -> source in
      let body' = rebind_vars vars body in
      let to_match = make_match source' (fun bind -> on_target bind body') else_ in
      DSL.(seq values (Var.Generated stricted_values) to_match) in
    let is_strict (p, _) = match p with
      | Ast.PVar(_) | Ast.PWildcard -> false
      | _ -> true in
    let is_var (p, _) = match p with
      | Ast.PVar(_) -> true
      | _ -> false in
    let clause bindings else_ (_, params, guard, body) =
      let pairs = List.zip_exn params bindings in
      let vars = List.filter pairs ~f:(fun (p, _) -> is_var p) in
      let strict = List.filter pairs ~f:(fun (p, _) -> is_strict p) in
      let body' = translate' body in
      if (List.length strict > 0) || Option.is_some guard
      then clause' vars strict else_ guard body'
      else rebind_vars vars body' in
    let clauses bindings instances =
      List.fold instances ~init:DSL.stop ~f:(clause bindings) in
    let is_no_stricts params = List.find params ~f:is_strict |> Option.is_none in
    List.map defs ~f:(function
        | [(ident, params, None, body)] when is_no_stricts params ->
          let params' = List.map params ~f:(function
              | (PVar n, pos) -> (Var.Handcrafted { ident = n; pos })
              | (PWildcard,_) -> (Var.Generated "_")
              | _ -> raise Util.TODO) in
          (ident, params', translate' body)
        | ((ident, params, _, _)::_ as instances) ->
          let arity = List.length params in
          let bindings = List.init arity ~f:(fun _ -> make_fresh ()) in
          (ident, List.map bindings ~f:(fun binding -> Var.Generated binding), clauses bindings instances)
        | [] -> assert false) in
  (* Sexp.to_string_hum (Ast.sexp_of_e' e ) |> Stdio.print_endline; *)
  match e with
  | A.EIdent(v) -> (EIdent v, ast)
  | A.EOtherwise(e1, e2) ->
    (EOtherwise(translate' e1, translate' e2), ast)
  | A.EParallel(e1, e2) ->
    (EParallel(translate' e1, translate' e2), ast)
  | A.EPruning(e1, ((A.PVar ident), pos), e2) ->
    (EPruning(translate' e1, Some (Var.Handcrafted { ident; pos}), translate' e2), ast)
  | A.EPruning(e1, ((A.PWildcard), _), e2) ->
    (EPruning(translate' e1, None, translate' e2), ast)
  | A.EPruning(e1, p, e2) ->
    let source_binding = make_fresh () in
    let bridge = make_fresh () in
    let (source, on_target) = translate_pattern p source_binding in
    DSL.(prune
           (on_target bridge (translate' e1))
           (Var.Generated bridge)
           (seq (translate' e2) (Var.Generated source_binding) source))
  | A.ESequential(e1, ((A.PVar ident), pos), e2) ->
    (ESequential(translate' e1, Some (Var.Handcrafted {ident; pos}), translate' e2), ast)
  | A.ESequential(e1, ((A.PWildcard), _), e2) ->
    (ESequential(translate' e1, None, translate' e2), ast)
  | A.ESequential(e1, p, e2) ->
    let source_binding = make_fresh () in
    let bridge = make_fresh () in
    let (source, on_target) = translate_pattern p source_binding in
    DSL.(seq
           (translate' e1)
           (Var.Generated source_binding)
           (seq source (Var.Generated bridge) (on_target bridge (translate' e2))))
  | A.EList(es) ->
    deflate_many es (fun es' ->
        (EFFC("core.make-list", es'), ast))
  | A.ETuple(es) ->
    deflate_many es (fun es' ->
        (EFFC("core.make-tuple", es'), ast))
  | A.ERecord(pairs) ->
    let (keys, vals) = List.unzip pairs in
    let key_consts = (List.map keys ~f:(fun k ->
        (A.EConst(A.String k), A.Pos.dummy_range))) in
    deflate_many key_consts (fun keys' ->
        deflate_many vals (fun vals' ->
            let args = List.fold2_exn keys' vals' ~init:[] ~f:(fun acc k v ->
                k::v::acc) in
            (EFFC("core.make-record", args), ast) ))
  | A.ECond(pred, then_, else_) ->
    DSL.(
      deflate pred (fun pred' ->
          par
            (seqw (ffc "core.ift" [pred']) (translate' then_))
            (seqw (ffc "core.iff" [pred']) (translate' else_))))
  | A.EConst c -> (EConst(c), ast)
  (* special case for unary operator '-' *)
  | A.ECall((EIdent "-", _), _, [arg]) ->
    deflate arg (fun arg' ->
        (EFFC("core.sub", [arg']), ast))
  | A.ECall(e, _, args) ->
    deflate e (fun e' ->
        deflate_many args (fun args' ->
            (ECall(e', args'), ast)))
  | A.EFFC(target, args) ->
    deflate_many args (fun args' ->
        (EFFC(target, args'), ast))

  | A.EStop -> (EStop, ast)
  | A.ELambda(_, params, _, body) ->
    let n = make_fresh () in
    let e' = A.EDecl((DDef(n, [], params, None, None, body), A.Pos.dummy_range),
                     (A.EIdent n, A.Pos.dummy_range)) in
    translate'((e', A.Pos.dummy_range))
  | A.EDecl((DDef _, _), _) | EDecl((DSig _, _), _) ->
    translate_defs (e, pos)
  | A.EDecl((DVal(p, val_e), _), e) ->
    translate' ((EPruning(e, p, val_e)), pos)
  | A.EFieldAccess(target, field) ->
    deflate target (fun t ->
        deflate (A.EConst(A.String field), A.Pos.dummy_range) (fun f ->
            (EFFC("core.field-access", [t; f]), ast)))
  | A.EDecl((DRefer(ns, fs), _), e) ->
    deps := ns::!deps;
    (ERefer(ns, fs, translate' e), ast)
  | A.EDecl((DSite(_ident, _definition), _), _e) ->
    raise Util.TODO
  | A.EDecl((DData(_ident, _, _constructors), _), _e) ->
    raise Util.TODO

  | A.EDecl((DInclude(_), _), _e) ->
    assert false
  | EDecl((DAlias(_), _), e) | EHasType(e, _) | EOverrideType(e, _) ->
    translate' e
  | EModule -> (EModule, ast)

and deflate e k =
  match e with
  | (Ast.EIdent(v), _) -> k v
  | ast ->  let f = make_fresh () in
    (EPruning(k f, Some (Var.Generated f), translate' e), ast)
and deflate_many es k =
  let rec step acc = function
    | e::es ->
      deflate e (fun i ->
          step (i::acc) es)
    | [] -> k (List.rev acc) in
  step [] es

let translate e =
  deps := [];
  let e' = translate' e in
  (!deps, e')

let translate_no_deps e =
  let (deps, e') = translate e in
  if List.length deps > 0
  then Error(`UnexpectedDependencies deps)
  else Ok(e')
