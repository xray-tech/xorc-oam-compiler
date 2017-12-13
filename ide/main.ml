open! Core_kernel
open Incr_dom

module List = struct
  module Assoc = struct
    include List.Assoc

    let update t ~equal ~f key =
      let old = Moption.create () in
      let l = List.filter t ~f:(fun (key', v) ->
          if (equal key key')
          then
            (Moption.set_some old v;
             false)
          else true) in
      match Moption.get old with
      | Some(v) -> (key, f v)::l
      | None -> t
  end

  include (List : module type of struct include List end
           with module Assoc := Assoc)
end

module Model = struct
  module Value = struct
    type t = { value : string;
               time_diff : int option}
    [@@deriving sexp, compare]
  end

  module Coeffect = struct
    type t = { id : int;
               definition : string;
               value : string;
               collapsed : bool }
    [@@deriving sexp, compare]
  end


  type t =
    | Init of { program : string;
                error : string option }
    | Running of { program : string;
                   values : Value.t list;
                   coeffects : (int * Coeffect.t) list;
                   realized : (string * string) list;
                   error : string option;
                   bc : Orcml.bc [@compare.ignore];
                   instance : Orcml.instance [@compare.ignore]}
    | Finished of { program : string;
                    values : Value.t list;
                    realized : (string * string) list}
  [@@deriving compare]

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Run
    | Upload of string
    | ProgramChanged of string
    | RealizeCoeffect of int * string
    | CoeffectValueChanged of int * string
    | CoeffectToggle of int
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let debug v = Firebug.console##log v

let with_error err (model : Model.t) =
  match model with
  | Init r -> Model.Init { r with error = Some(err)}
  | Running r -> Running { r with error = Some(err)}
  | Finished r -> Model.Init { error = Some(err); program = r.program }

let map_values values =
  List.map values ~f:(fun v ->
      { Model.Value.value = Orcml.Value.sexp_of_t v |> Sexp.to_string_hum;
        time_diff = None })

let map_coeffects coeffects =
  List.map coeffects ~f:(fun (id, v) ->
      (id, { Model.Coeffect.id;
             definition = Orcml.Value.sexp_of_t v |> Sexp.to_string_hum;
             value = "";
             collapsed = true}))

let run program (model : Model.t) bc =
  match Orcml.run bc with
  | Error(err) ->
    with_error (Error.to_string_hum err) model
  | Ok({Orcml.Res.values; instance; coeffects }) ->
    Running { instance; program; bc;
              values = map_values values;
              coeffects = map_coeffects coeffects;
              error = None;
              realized = [] }

let get_code (model : Model.t) =
  match model with
  | Init r -> r.program
  | Running r -> r.program
  | Finished r -> r.program

let run_code (model : Model.t) =
  let code = get_code model in
  let compiled =
    let open Result.Let_syntax in
    let%bind parsed = Orcml.parse code in
    debug (sprintf "Parsed:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum));
    let%bind ir1 = Orcml.translate_no_deps parsed in
    Orcml.compile ir1 in
  (match compiled with
   | Error(err) ->
     with_error (Error.to_string_hum err) model
   | Ok((_, repo)) ->
     run code model (Orcml.finalize repo))

let run_bc (model : Model.t) bc =
  let (_, packed) = Msgpck.String.read bc in
  match Orcml.Serializer.load packed with
  | Error(err) ->
    with_error (sprintf "Can't load bytecode, error: %s" (Error.to_string_hum err)) model
  | Ok(bc) ->
    run "" model bc

let set_code (model : Model.t) program =
  match model with
  | Init r -> Model.Init { r with program }
  | Running r -> Running { r with program }
  | Finished r -> Finished { r with program }

let coeffect_value_changed (model : Model.t) id v =
  match model with
  | Running({coeffects} as r) ->
    let coeffects = List.Assoc.update coeffects id ~equal:Int.equal ~f:(fun c ->
        { c with value = v }) in
    Model.Running { r with coeffects }
  | other -> other

let coeffect_toggle (model : Model.t) id =
  match model with
  | Running({coeffects} as r) ->
    let coeffects = List.Assoc.update coeffects id ~equal:Int.equal ~f:(fun c ->
        { c with collapsed = not c.collapsed }) in
    Model.Running { r with coeffects }
  | other -> other

let realize_coeffect (model : Model.t) id v =
  match model with
  | Running({coeffects; values; realized; instance; bc; program} as r) ->
    let coeffects' = List.Assoc.remove coeffects ~equal:Int.equal id in
    let res = let open Or_error.Let_syntax in
      let%bind v' = Orcml.parse_value v in
      Orcml.unblock bc instance id v' in
    (match res with
     | Ok({Orcml.Res.instance; coeffects = new_coeffects; values = new_values; killed}) ->
       let coef = List.Assoc.find_exn coeffects ~equal:Int.equal id in
       let coeffects'' = List.fold killed ~init:coeffects' ~f:(fun res id ->
           List.Assoc.remove res ~equal:Int.equal id) in
       let values' = values @ (map_values new_values) in
       let realized_coef = (coef.definition, v) in
       let realized' = realized_coef::realized in
       if List.is_empty coeffects''
       then (Model.Finished { values = values';
                              program;
                              realized = realized'})
       else (Running { r with values = values';
                              realized = realized';
                              coeffects = coeffects'' @ (map_coeffects new_coeffects);
                              instance})
     | Error(err) -> with_error (Error.to_string_hum err) model)
  | other -> other


let apply_action action model _state =
  match (action:Action.t) with
  | Run -> run_code model
  | ProgramChanged(p) -> set_code model p
  | Upload(bc) -> run_bc model bc
  | CoeffectValueChanged((id, v)) -> coeffect_value_changed model id v
  | CoeffectToggle(id) -> coeffect_toggle model id
  | RealizeCoeffect((id, v)) -> realize_coeffect model id v

let update_visibility m = m

let on_startup ~schedule:_ _ =
  Async_kernel.return ()

let on_display ~old:_ _ _ = ()

module View = struct
  open Vdom
  open Attr
  open Node


  let main inject (m : Model.t) =
    let error_row = function
      | None -> []
      | Some(err) ->
        [div
           [classes ["row"; "a-error"]]
           [div [class_ "col"] [text err]]] in

    let coeffect_row (_, {Model.Coeffect.definition; id; collapsed; value}) =
      let on_change = on_change (fun _ t -> inject (Action.CoeffectValueChanged(id, t))) in
      let on_toggle = on_click (fun _ -> inject (Action.CoeffectToggle id)) in
      let on_realize = on_click (fun _ -> inject (Action.RealizeCoeffect(id, value))) in
      if collapsed
      then
        (div [classes ["row"; "a-coeffect"]]
           [div [class_ "col"]
              [create "pre" [] [text definition]];
            div [class_ "col"]
              [button [on_toggle] [text "Realize"]]])
      else
        (div [classes ["row"; "a-coeffect"]]
           [div [class_ "col"]
              [div [class_ "row"]
                 [div [class_ "col"]
                    [create "pre" [] [text definition]]];
               div [class_ "row"]
                 [div [class_ "col"]
                    [text "=>"]];
               div [class_ "row"]
                 [div [class_ "col"]
                    [input [on_change; Attr.value value] []]];
               div [class_ "row"]
                 [div [class_ "col"]
                    [button [on_realize] [text "Unblock"];
                     button [on_toggle] [text "Cancel"]]]]]) in

    let editor_row program coeffects _is_running =
      let on_change = on_change (fun _ t -> inject (Action.ProgramChanged t)) in
      let on_run = on_click (fun _ -> inject Action.Run) in

      [div [classes ["row"]]
         [div [classes ["col"; "a-editor"]]
            [textarea
               [on_change;
                classes ["w-100"];
                string_property "rows" "10"]
               [text program]];
          div [classes ["col"; "a-editor-side"]]
            ([div [classes ["row"; "a-buttons"]]
                [div [class_ "col"]
                   [button [on_run] [text "Run"]]]]
             @ if List.is_empty coeffects
             then []
             else
               [div [classes ["row"; "a-coeffects-title"]]
                  [div [class_ "col"] [text "Coeffects"]];
                div [classes ["row"; "a-coeffects"]]
                  [div [class_"col"]
                     (List.map coeffects ~f:coeffect_row)]])]] in

    let result_rows values _realized =
      [div [classes ["row"; "a-values"; "a-result"]]
         [div [class_ "col"]
            ([div [class_ "row"]
                [div [classes ["col"; "a-title"]]
                   [text "Values"]]]
             @ List.map values ~f:(fun {Model.Value.value} ->
                 div [class_ "row"]
                   [div [class_ "col"]
                      [create "pre" []
                         [text value]]]))]] in
    match m with
    | Init { program; error } ->
      error_row error @ editor_row program [] false
    | Running { program; values; coeffects; error; realized } ->
      error_row error
      @ editor_row program coeffects true
      @ result_rows values realized
    | Finished { program; values; realized } ->
      editor_row program [] false
      @ result_rows values realized

end

(* let values = Node.ul [] (List.map m.Model.values ~f:(fun v ->
 *     Node.li [] [(Node.text v)])) in *)

(* let on_drop = Attr.on "drop" (fun e ->
 *     let files = e##.dataTransfer##.files in
 *     (if files##.length > 0
 *      then
 *        let reader = new%js File.fileReader in
 *        reader##.onloadend := Dom.handler (fun _ ->
 *            let content = reader##.result
 *                          |> File.CoerceTo.arrayBuffer
 *                          |> Js.Opt.to_option
 *                          |> Option.value_exn
 *                          |> Typed_array.String.of_arrayBuffer in
 *            inject (Upload content) |> ignore;
 *            Js._false);
 *        reader##readAsArrayBuffer(files##item(0)));
 *     Event.Many [Event.Prevent_default; Event.Stop_propagation];
 *   ) in *)

let view (m : Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let%map m = m in
  let open Vdom in
  let body = Node.div
      [Attr.class_ "container"]
      (View.main inject m) in
  Node.body [] [body]
