open Base

type prim = Let | Add [@@deriving compare, enumerate]

exception UnboundVar of string

type binding =
  | BindVar of string
  | BindFun of string * int
  | BindPrim of string * prim [@@deriving variants]

type state = { mutable code : (int * Inter.t list) list }

let prims_map = [("Let", Let); ("+", Add)]

let prims = List.map prims_map (fun ((s, p)) -> BindPrim(s, p))

let list_index l v ~f =
  let rec index acc = function
    | [] -> None
    | x::xs when f v x = 0 -> Some(acc)
    | _::xs -> index (acc + 1) xs in
  index 0 l

let ctx_find ctx ident =
  let rec find acc = function
    | [] -> raise (UnboundVar ident)
    | BindVar(ident')::_ when String.equal ident ident' ->
      (acc, BindVar ident')
    | BindVar(_)::xs -> find (acc + 1) xs
    | BindFun(ident', c)::xs when String.equal ident ident' ->
      (acc, BindFun(ident', c))
    | BindFun(_, _)::xs -> find acc xs
    | BindPrim(ident', p)::_ when String.equal ident ident' ->
      (acc, BindPrim(ident', p))
    | BindPrim(_,_)::xs -> find acc xs in
  find 0 ctx

let new_index ctx  =
  let rec find acc = function
    | [] -> acc
    | BindVar(_)::xs -> find (acc + 1) xs
    | BindFun(_)::xs | BindPrim(_)::xs -> find acc xs in
  find 0 ctx

let prim_index p =
  match list_index all_of_prim p compare_prim with
  | Some i -> i
  | None -> assert false

let prim_target p = Inter.TPrim(prim_index p)

let len = List.length

let rec compile_e state ctx e =
  let open Ir1 in
  match e with
  | EIdent x ->
    (match ctx_find ctx x with
     | (i, BindVar(_)) -> (0, [Inter.Call(prim_target Let, [| i |])])
     | (_, BindFun(_, i)) -> (0, [Inter.Closure(i)]))
  | EConst v -> (0, [Inter.Const v])
  | EParallel(e1, e2) ->
    let (s1, e1') = compile_e state ctx e1 in
    let (s2, e2') = (compile_e state ctx e2) in
    let e2'' = e2' @ e1' in
    ((Int.max s1 s2),
     Inter.Parallel(len e1' - 1, len e2'' - 1)::e2'')
  | EPruning(e1, v, e2) ->
    let ctx' = BindVar(v)::ctx in
    let (s1, e1') = compile_e state ctx' e1 in
    let (s2, e2') = compile_e state ctx e2 in
    let e2'' = e2' @ e1' in
    ((Int.max s1 s2) + 1,
     Inter.Pruning(len e1' - 1, new_index ctx, len e2'' -1)::e2'')
  | EFix(fs, e) ->
    (* TODO closures *)
    let names = List.map fs (fun (n, _, _) -> BindFun(n, i)) in
    let ctx' = names @ ctx  in
    let fs' = List.map fs (fun (ident, params, body) ->
        (ident, compile_fun state ctx' params body)) in
    compile_e state ctx' e
  | ECall(ident, args) ->
    let args' = List.map args (fun arg ->
        match ctx_find ctx arg with
        | (i, BindVar(_)) -> i) in
    (match ctx_find ctx ident with
     | (_, BindPrim(_, p)) ->
       (0, [Inter.Call(Inter.TPrim(prim_index p), Array.of_list args')])
     | (_, BindFun(_, c)) ->
       (0, [Inter.Call(Inter.TFun(c), Array.of_list args')]))
and compile_fun state ctx params body =
  let i = List.length state.code in
  let params' = List.map params bindvar in
  let ctx' = params' @ ctx  in
  let (env_size, body) = compile_e state ctx' body in
  state.code <- (env_size + List.length params, body)::state.code;
  i

let compile e =
  let state = { code = [] } in
  compile_fun state prims [] e |> ignore;
  Array.of_list_rev_map state.code ~f:(fun (s, code) ->
      (s, Array.of_list_rev code))
