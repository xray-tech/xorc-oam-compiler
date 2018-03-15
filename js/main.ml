open Base
open Orcml

let jsify_error err =
  let kind = match err with
    | `NoInput -> "noinput"
    | `SyntaxError _ -> "syntax-error"
    | `UnboundVar _ -> "unbound-var"
    | `UnknownReferedFunction _ -> "unknown-refered-funtion"
    | `UnknownFFI _ -> "unknown-ffi" in
  let msg = error_to_string_hum err in
  object%js val kind = kind val msg = msg end

let jsify_result = function
  | Ok v -> object%js
    val ok = Js.some v
    val error = Js.null
  end
  | Error err -> object%js
    val ok = Js.null
    val error = Js.some err
  end

let or_error x =
  x
  |> Result.map_error ~f:jsify_error
  |> jsify_result

let inject = Js.Unsafe.inject

(* let jsify_const = function
 *   | Const.Int x -> x |> inject
 *   | Float x -> x |> inject
 *   | String x -> Js.string x |> inject
 *   | Signal -> (object%js val signal = true end) |> inject
 *   | Null -> Js.null |> inject
 *   | Bool x -> x |> inject
 * 
 * let jsify_value = function
 *   | Value.VConst x -> jsify_const x
 *   | VClosure _ as v | VLabel _ -> inject v
 *   | _ -> Js.string "other" |> inject *)

let jsify_inter_res res =
  object%js
    (* List.map ~f:jsify_value *)
    val values = res.Res.values |> Array.of_list |> Js.array
  end

let () =
  Js.export "Orcml"
    (object%js
      method makeRepository = Repository.create ()
      method compile repository s =
        compile ~repository (Js.to_string s) |> or_error
      method inter bc =
        inter bc |> or_error
      method run inter =
        run inter |> jsify_inter_res

      method valueToString x = Value.to_string x |> Js.string
    end)
