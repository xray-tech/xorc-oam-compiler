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
  read_msg r
  >>= function
  | Some(M.List [M.List values; M.List coeffects; M.List killedCoeffects]) ->
    Some((Orcml.Serialize.deserialize_values (fun _ -> assert false) values),
     List.map killedCoeffects (function
         | M.Int i -> i
         | _ -> assert false))
    |> return
  | None -> return None
  | Some(invalid) ->
    printf "Bad protocol message: %s" (Message_pack.sexp_of_t invalid |> Sexp.to_string_hum);
    exit 1

module M = Msgpck

let write w v =
  Writer.write w (M.String.to_string v)

let write_code w code =
  write w (M.Int 0);
  Writer.write w (Orcml.Serialize.serialize_bc code)

let write_unblock w id v =
  write w (M.Int 1);
  write w (M.List ((M.Int id)::(Orcml.Serialize.serialize_value (fun _ -> assert false) v)))

let write_bench w code n =
  write w (M.Int 2);
  write w (M.Int n);
  Writer.write w (Orcml.Serialize.serialize_bc code)
