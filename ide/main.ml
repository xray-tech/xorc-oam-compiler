open! Core_kernel
open Incr_dom

module Model = struct
  type t = {
    program : string;
    values : string list;
  } [@@deriving sexp, fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t =
    | Run
    | ProgramChanged of string
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let debug v = Firebug.console##log v

let apply_action action model _state =
  match (action:Action.t) with
  | Run ->
    let code = Model.program model in
    let compiled =
      let open Result.Let_syntax in
      let%bind parsed = Orcml.parse code in
      debug (sprintf "Parsed:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum));
      let%bind ir1 = Orcml.translate_no_deps parsed in
      Orcml.compile ir1 in
    (match compiled with
     | Error(err) ->
       debug (sprintf "Compile error:%s" (Error.to_string_hum err));
       model
     | Ok((_, repo)) ->
       let bc = Orcml.finalize repo in
       (match Orcml.run bc with
        | Error(err) ->
          debug (sprintf "Runtime error:%s" (Error.to_string_hum err));
          model
        | Ok(res) ->
          let values' = List.map res.Orcml.Res.values ~f:(fun v ->
              Orcml.Value.sexp_of_t v |> Sexp.to_string_hum) in
          Model.{model with values = values'}))
  | ProgramChanged(p) ->
    Model.{ model with program = p }

let update_visibility m = m

let on_startup ~schedule:_ _ =
  Async_kernel.return ()

let on_display ~old:_ _ _ = ()

let view (m : Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let on_change = Attr.on_change (fun _ t -> inject (Action.ProgramChanged t)) in
  let on_run = Attr.on_click (fun _ -> inject Action.Run) in
  let on_drop = Attr.on "drop" (fun e ->
      let files = e##.dataTransfer##.files in
      (if files##.length > 0
       then
         let reader = new%js File.fileReader in
         reader##.onloadend := Dom.handler (fun _ ->
             let content = reader##.result
                           |> File.CoerceTo.arrayBuffer
                           |> Js.Opt.to_option
                           |> Option.value_exn
                           |> Typed_array.String.of_arrayBuffer in
             let (_, packed) = Msgpck.String.read content in
             match Orcml.Serializer.load packed with
             | Error(err) ->
               debug (sprintf "Can't load bytecode, error: %s" (Error.to_string_hum err));
               Js._false
             | Ok(bc) ->
               debug (Orcml.sexp_of_bc bc |> Sexp.to_string_hum);

               Js._false);
         reader##readAsArrayBuffer(files##item(0)));
      Event.Many [Event.Prevent_default; Event.Stop_propagation];
    ) in
  let%map m = m in
  let editor = Node.div [] [
      Node.textarea [on_change;
                     Attr.style [("width", "70%")];
                     Attr.string_property "rows" "10"]
        [Node.text m.Model.program];
      Node.button [on_run] [Node.text "Run"]] in
  let values = Node.ul [] (List.map m.Model.values ~f:(fun v ->
      Node.li [] [(Node.text v)])) in

  let body = Node.div
      [Attr.class_ "container";
       on_drop]
      [editor; values] in
  Node.body [] [body]
