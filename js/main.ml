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
  end |> Js.Unsafe.inject
  | Error err -> object%js
    val error = Js.some err
  end |> Js.Unsafe.inject

let or_error x =
  x
  |> Result.map_error ~f:jsify_error
  |> jsify_result

let inject = Js.Unsafe.inject

let jsify_const = function
  | Const.Int x -> x |> inject
  | Float x -> x |> inject
  | String x -> Js.string x |> inject
  | Signal -> (object%js val signal = true end) |> inject
  | Null -> Js.null |> inject
  | Bool x -> x |> inject

let jsify_value = function
  | Value.VConst x -> jsify_const x
  | VClosure _ -> (object%js val closure = true end) |> inject
  | VLabel _ -> (object%js val label = true end) |> inject
  | _ -> Js.string "other" |> inject

let jsify_inter_res res =
  object%js
    val values = res.Res.values |> List.map ~f:jsify_value |> Array.of_list |> Js.array
  end

let jsify_env_value x = match x with
  | Debugger.Value v -> object%js
    val value = jsify_value v
    val original = x
  end |> Js.Unsafe.inject
  | Debugger.Pending _ -> object%js
    val pending = true
    val original = x
  end |> Js.Unsafe.inject

let jsify_pos {path; line; col} =
  object%js
    val path = Js.string path
    val line = line
    val col = col
  end

let jsify_range {start; finish} =
  object%js
    val start = jsify_pos start
    val finish = jsify_pos finish
  end

let jsify_var = function
  | Debugger.Var.Generated x -> x |> Js.Unsafe.inject
  | Debugger.Var.Handcrafted { index; ident; pos } ->
    object%js
      val index = index
      val ident = ident |> Js.string
      val pos = pos |> jsify_range
    end |> Js.Unsafe.inject

let jsify_env_cell (var, v) =
  object%js
    val var = jsify_var var
    val value = jsify_env_value v
  end

let jsify_thread ({Debugger.id; env; pos} as thread) =
  object%js
    val id = id
    val env = env |> Array.map ~f:jsify_env_cell |> Js.array
    val pos = pos
    val original = thread
  end

let jsify_threads threads =
  Array.of_list threads |> Array.map ~f:jsify_thread |> Js.array

let jsify_action = function
  | Debugger.PublishedValue v ->
    object%js val publishedValue = jsify_value v end |> Js.Unsafe.inject
  | NewThread id ->
    object%js val newThread = id end |> Js.Unsafe.inject
  | HaltedThread id ->
    object%js val haltedThread = id end |> Js.Unsafe.inject
  | Coeffect { thread; id; desc } ->
    object%js
      val coeffect = id
      val thread = thread
      val desc = jsify_value desc
    end |> Js.Unsafe.inject
  | Error { thread; ffi; args } ->
    object%js
      val ffiError = Js.string ffi
      val thread = thread
      val args = Array.of_list args |> Array.map ~f:jsify_value
    end |> Js.Unsafe.inject

let jsify_trace trace =
  Array.of_list trace |> Array.map ~f:jsify_action |> Js.array

let () =
  Js.export "Orcml"
    (object%js
      method makeRepository = Repository.create ()
      method compile repository s =
        compile ~repository (Js.to_string s) |> or_error
      method inter bc =
        inter bc
        |> Result.map ~f:(fun inter ->
            (object%js
              method run =
                run inter |> jsify_inter_res
              val original = inter
            end))
        |> or_error

      val debugger = object%js
        method init inter =
          let (state, threads) = Debugger.init inter##.original in
          object%js
            val state = state
            val threads = jsify_threads threads
          end
        method tick state thread =
          let (threads, trace) = Debugger.tick state thread##.original in
          object%js
            val threads = jsify_threads threads
            val trace = jsify_trace trace
          end
      end
    end)
