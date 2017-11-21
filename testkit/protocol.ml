open! Core
open! Async

exception ProtocolError

let read_msg r =
  let handle_chunk buf ~pos ~len =
    Result.try_with (fun () -> Msgpck.String.read (Bigstring.to_string buf))
    |> (function
        | Ok((consumed, msg)) -> `Stop_consumed (msg, consumed)
        | Error(Invalid_argument(_)) -> `Continue
        | Error(exn) -> raise exn)
    |> return in
  Reader.read_one_chunk_at_a_time r ~handle_chunk
  >>| function
  | `Eof | `Eof_with_unconsumed_data _ -> raise ProtocolError
  | `Stopped v ->
    printf "Msg: %s\n" (Message_pack.sexp_of_t v |> Sexp.to_string_hum);
    v

let read_res r =
  let module M = Msgpck in
  read_msg r
  >>= function
  | M.List [M.List values; M.List coeffects; M.List killedCoeffects] ->
    ((Orcml.Serialize.deserialize_values (fun _ -> assert false) values),
     List.map killedCoeffects (function
         | M.Int i -> i
         | _ -> assert false))
    |> return
  | invalid ->
    printf "Bad protocol message: %s" (Message_pack.sexp_of_t invalid |> Sexp.to_string_hum);
    exit 1

let write_code w code =
  let module M = Msgpck in
  let v = (M.String.to_string (M.Int 0)) in
  print_endline v;
  Writer.write w v;
  Writer.write w (Orcml.Serialize.serialize_bc code)

let write_unblock w id v =
  let module M = Message_pack in
  let write v = Writer.write w (M.String.to_string v) in
  write (M.Int 1);
  write (M.Int id);
  List.iter ~f:write (Orcml.Serialize.serialize_value (fun _ -> assert false) v)
