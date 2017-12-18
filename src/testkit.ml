open Common
type msg = Execute of Inter.bc
         | Continue of int * Inter.v
         | Benchmark of Inter.bc * int

type res = { values : Inter.v list;
             coeffects : Inter.coeffect list;
             killed : int list }


module Serializer = struct
  type load_error =
  [ `BadFormat ]

  module M = Msgpck
  let dump_msg = function
    | Execute(bc) ->
      M.List [M.Int 0; Serializer.dump bc]
    | Continue(id, v) ->
      M.List ((M.Int 1)::(M.Int id)::(Serializer.dump_value (fun _ -> assert false) v))
    | Benchmark(bc, iter) ->
      M.List [M.Int 2; Serializer.dump bc; M.Int iter]

  let load_msg = function
    | M.List [M.Int 0; bc] ->
      Result.map (Serializer.load_no_linker bc) ~f:(fun v ->
          Execute(v))
    | M.List ((M.Int 1)::(M.Int id)::tail) ->
      Result.map (Serializer.load_value (fun _ -> assert false) tail) ~f:(fun (v, _) ->
          Continue(id, v))
    | M.List [M.Int 2; bc; M.Int iter] ->
      Result.map (Serializer.load_no_linker bc) ~f:(fun v ->
          Benchmark(v, iter))
    | _ -> Error(`BadFormat)

  let dump_res {Inter.Res.values; coeffects; killed} =
    let values' = List.map values ~f:(fun v ->
        M.List (Serializer.dump_value (fun _ -> assert false) v)) in
    let dump_coeffect (id, v) =
      M.List ((M.Int id)::(Serializer.dump_value (fun _ -> assert false) v)) in
    M.List [
      M.List values';
      M.List (List.map coeffects ~f:dump_coeffect);
      M.List (List.map killed ~f:(fun x -> M.Int x))]

  let load_res v =
    with_return (fun r ->
        let bad_format () = r.return (Error `BadFormat) in
        match v with
        | M.List [M.List values; M.List coeffects; M.List killed] ->
          let values' = List.map values ~f:(function
              | M.List xs ->
                (match Serializer.load_value (fun _ -> assert false) xs with
                 | Ok((v, _)) -> v
                 | _ -> bad_format ())
              | _ -> bad_format ()) in
          let coeffects' = List.map coeffects ~f:(function
              | M.List ((M.Int id)::xs) ->
                (match Serializer.load_value (fun _ -> assert false) xs with
                 | Ok((v, _)) -> (id, v)
                 | _ -> bad_format ())
              | _ -> bad_format ()) in
          let killed' = List.map killed ~f:(function
              | M.Int v -> v
              | _ -> bad_format ()) in
          Ok { values = values';
               coeffects = coeffects';
               killed = killed'}
        | _ -> bad_format ())

  let dump_bench_res res =
    M.Float res

  let load_bench_res = function
    | M.Float v -> Ok(v)
    | _ -> (Error `BadFormat)
end
