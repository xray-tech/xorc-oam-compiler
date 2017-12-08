open! Core
open! Async
open! Log.Global

exception ProtocolError

let read_msg r =
  let handle_chunk buf ~pos ~len =
    Result.try_with (fun () -> Msgpck.String.read (Bigstring.to_string ~pos ~len buf))
    |> (function
        | Ok((consumed, msg)) ->
          `Stop_consumed (msg, consumed)
        | Error(Invalid_argument(_)) -> `Continue
        | Error(exn) -> raise exn)
    |> return in
  Reader.read_one_chunk_at_a_time r ~handle_chunk
  >>| function
  | `Eof | `Eof_with_unconsumed_data _ -> None
  | `Stopped v ->
    debug  "Msg: %s\n" (Message_pack.sexp_of_t v |> Sexp.to_string_hum);
    Some(v)

let read_msg_or_exit ~code r =
  read_msg r >>= function
  | Some(v) -> return v
  | None -> exit code

let read_res r =
  let module M = Msgpck in
  let%bind msg = read_msg r in
  match msg with
  | None ->
    error "Stream was closed";
    exit 1
  | Some(msg) ->
    match Orcml.Testkit.Serializer.load_res msg with
    | Ok(v) -> return v
    | Error(err) ->
      error "Protocol error: %s" (Error.to_string_hum err);
      exit 1

module M = Msgpck

let write w v =
  Writer.write w (M.String.to_string v)
