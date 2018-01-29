open! Core
open! Async
open! Log.Global

let set_logging verbose =
  (if verbose then set_level `Debug);
  let output = (Async_extended.Extended_log.Console.output (Lazy.force Writer.stderr)) in
  set_output [output]

let verbose_flag =
  let open Command.Param in
  flag "verbose" no_arg ~doc:"Verbose logging"

let read_file_or_stdin = function
  | None -> Reader.contents (Lazy.force Reader.stdin)
  | Some(f) -> Reader.file_contents f

module type NSLoader = sig
  val load : string -> string option Deferred.t
end

let list_position l ~f =
  let rec step acc = function
    | [] -> None
    | x::_ when f x -> Some(acc)
    | _::xs -> step (acc + 1) xs in
  step 0 l

let compile_namespaces (module NSLoader : NSLoader) init_queue =
  let compiled = ref [] in
  let is_already_compiled ns =
    List.find !compiled ~f:(fun (ns', _) -> String.equal ns ns') |> Option.is_some in
  let analyze filename code =
    let open Result.Let_syntax in
    let%map parsed = Orcml.parse_ns ~filename code in
    debug "Parsed NS:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum);
    Orcml.translate parsed in
  let rec compile_loop = function
    | [] ->
      Ok() |> return
    | ns::queue when is_already_compiled ns ->
      compile_loop queue
    | ns::queue ->
      match%bind NSLoader.load ns with
      | None -> Error(`CantLoadNS ns) |> return
      | Some(code) ->
        analyze ns code |> function
        | Error(err) -> Error(err) |> return
        | Ok((deps, e)) ->
          compile_loop deps >>=? fun () ->
          compiled := (ns, e)::!compiled;
          compile_loop queue in
  compile_loop init_queue >>=? (fun () ->
      return (Ok !compiled))

let optional_file_contents path =
  Monitor.try_with (fun () -> Reader.file_contents path >>| Option.some )
  >>| function
  | Ok(v) -> v
  | Error(_) -> None

let file_contents path =
  let%map res = Monitor.try_with (fun () -> Reader.file_contents path ) in
  Result.map_error res ~f:(fun e -> Error.of_exn e )

let fs path =
  (module struct
    let load ns =
      let ns' = String.Search_pattern.(replace_all (create "\\.") ~in_:ns ~with_:"/") ^ ".orc" in
      optional_file_contents (Filename.concat path ns')
  end : NSLoader)

let multiloader loaders =
  (module struct
    let load ns = Deferred.List.find_map loaders ~f:(fun (module Loader : NSLoader) ->
        Loader.load ns)
  end : NSLoader)

let empty_loader =
  (module struct
    let load _ns = return None
  end : NSLoader)

let implicit_prelude =
  "refer from core (abs, signum, min, max, (+), (-), (*), (/), (%), (**), (=), (/=),
     (:>), (>=), (<:), (<=), (||), (&&), (~), (:), Ift, Iff, ceil, floor, sqrt, Let,
     Rwait, Println)
   refer from idioms (curry, curry3, uncurry, uncurry3, flip, constant, defer, defer2,
     ignore, ignore2, compose, while, repeat, fork, forkMap, seq, seqMap, join,
     joinMap, alt, altMap, por, pand)
   refer from list (each, map, reverse, filter, head, tail, init, last, empty, index,
     append, foldl, foldl1, foldr, foldr1, afold, zipWith, zip, unzip, concat,
     length, take, drop, member, merge, mergeBy, sort, sortBy, mergeUnique,
     mergeUniqueBy, sortUnique, sortUniqueBy, group, groupBy, rangeBy, range,
     any, all, sum, product, and, or, minimum, maximum)"

let compile_source loader prelude prog =
  let prog' = if prelude
    then implicit_prelude ^ prog
    else prog in
  let analyzed =
    let open Result.Let_syntax in
    let%map parsed = Orcml.parse prog' in
    debug "Parsed:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum);
    Orcml.translate parsed in
  match analyzed with
  | Error(err) -> Error(err) |> return
  | Ok((deps, ir)) ->
    debug "Translated:\n%s" (Orcml.sexp_of_ir1 ir |> Sexp.to_string_hum);
    compile_namespaces loader deps
    >>=? fun deps ->
    match Orcml.compile ~deps ir with
    | Error _ as err -> return err
    | Ok(bc) ->
      debug "Compiled:\n%s" (Orcml.sexp_of_bc bc |> Sexp.to_string_hum);
      return (Ok bc)

let compile_bc prog =
  let (_, packed) = Msgpck.String.read prog in
  return (Orcml.Serializer.load packed)

let compile_input prelude includes is_byte_code input =
  let loader = multiloader (List.map includes ~f:fs) in
  let%bind prog = read_file_or_stdin input in
  if is_byte_code
  then compile_bc prog
  else compile_source loader prelude prog

let make_bytecode prelude includes input =
  let loader = multiloader (List.map includes ~f:fs) in
  let%bind prog = read_file_or_stdin input in
  match%map compile_source loader prelude prog with
  | Error _ as err -> err
  | Ok(bc) -> Ok (Orcml.Serializer.dump bc)

let prelude_flag =
  let open Command.Param in
  flag "-prelude" no_arg ~doc:"Implicity refer whole prelude"

let includes_flag =
  let open Command.Param in
  flag "-i" (listed file) ~doc:"Directories to include"

let exts_flag =
  let open Command.Param in
  flag "-ext" (listed file) ~doc:"Extensions"

let bc_flag =
  let open Command.Param in
  flag "-bc" no_arg ~doc:"Execute bytecode, not Orc source file. By default reads from stdin"

let dump_flag =
  let open Command.Param in
  flag "-dump" (optional file) ~doc:"Path to store intermediate state if any"

let error_to_string_hum = function
  | `CantLoadNS ns -> (sprintf "Can't load namespace %s" ns)
  | (`NoInput | `SyntaxError _ | `UnboundVar _ | `BadFormat | `UnsupportedValueAST | `UnknownFFI _ | `UnknownReferedFunction _) as other ->
    Orcml.error_to_string_hum other

let compile =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "produce bytecode"
    [%map_open
      let input = anon (maybe ("INPUT" %: file))
      and output = flag "-output" (optional file) ~doc:"Output path"
      and includes = includes_flag
      and prelude = prelude_flag
      and verbose = verbose_flag in
      let exec () =
        make_bytecode prelude includes input >>= fun res ->
        (match res with
         | Error(err) ->
           error "Can't compile: %s" (error_to_string_hum err);
           exit 1
         | Ok(bc) ->
           let bc' = Msgpck.String.to_string bc in
           (match output with
            | Some(path) -> Writer.save path ~contents:bc'
            | None ->
              print_string bc'; Async.return ()) >>= fun () ->
           exit 0) in
      fun () ->
        set_logging verbose;
        exec () |> ignore;
        Scheduler.go () |> never_returns]

let run_loop state_path unblock res =
  let on_air = ref 0 in
  let stopped = Ivar.create () in
  let minstance = Moption.create () in
  let module V = Orcml.Value in
  let module C = Orcml.Const in
  let dump_state instance =
    match state_path with
    | Some(state_path) ->
      let msgpck = Orcml.Serializer.dump_instance instance in
      Writer.save state_path ~contents:(Msgpck.String.to_string msgpck)
    | _ -> return () in
  let coeffect_kind = function
    | V.VRecord(pairs) ->
      (match List.Assoc.find pairs ~equal:String.equal "kind" with
       | Some(V.VConst(C.String v)) -> Some(v, pairs)
       | _ -> None)
    | _ -> None in
  let rec handle_rwait id r =
    match List.Assoc.find_exn r ~equal:String.equal "timeout" with
    | V.VConst(C.Int(v)) ->
      let timeout = Float.of_int v |> Time.Span.of_ms in
      after timeout >>= fun () ->
      on_air := !on_air - 1;
      unblock' id (V.VConst C.Signal)
    | v ->
      error "Bad type for timeout value: %s" (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum);
      return ()
  and handle_println id r =
    let v = List.Assoc.find_exn r ~equal:String.equal "value" in
    info "Println: %s" (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum);
    on_air := !on_air - 1;
    unblock' id (V.VConst C.Signal)
  and handlers = [
    ("rwait", handle_rwait);
    ("println", handle_println)]
  and coeffect_handler v =
    let open Option.Let_syntax in
    let%bind (kind, pairs) = coeffect_kind v in
    let%map handler = List.Assoc.find handlers ~equal:String.equal kind in
    (handler, pairs)
  and tick {Orcml.Res.values; coeffects; instance} =
    Moption.set_some minstance instance;
    List.iter values ~f:(fun v ->
        info "Value: %s" (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum));
    on_air := !on_air + List.length coeffects;
    List.iter coeffects ~f:(fun (id, v) ->
        match coeffect_handler v with
        | Some(handler, pairs) ->
          handler id pairs |> don't_wait_for
        | None ->
          on_air := !on_air - 1;
          info "Coeffect: %i -> %s" id (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum));
    match (Orcml.is_running instance, !on_air) with
    | (false, _) ->
      Ivar.fill_if_empty stopped true; return ()
    | (true, 0) ->
      dump_state instance >>| fun () ->
      Ivar.fill_if_empty stopped true
    | (true, _) -> return ()
  and unblock' id v =
    match unblock (Moption.get_some_exn minstance) id v with
    | Error(err) ->
      error "Error: %s" (Error.sexp_of_t err |> Sexp.to_string_hum);
      exit 1
    | Ok(res) -> tick res in
  tick res |> don't_wait_for;
  let%bind res = Ivar.read stopped in
  if res
  then exit 0
  else exit 1

let compile_input_and_deps prelude includes bc input  =
  compile_input prelude includes bc input >>= fun res ->
  (match res with
   | Error(err) ->
     error "Can't compile: %s" (error_to_string_hum err);
     exit 1
   | Ok(bc) ->
     match Orcml.inter bc with
     | Error(err) ->
       error "Can't make runner: %s" (error_to_string_hum err);
       exit 1
     | Ok(inter) ->
       return inter)

let load_exts exts =
  List.iter exts ~f:(fun ext ->
      try Dynlink.loadfile ext with
      | Dynlink.Error err -> error "Ext %s load error: %s" ext (Dynlink.error_message err))

let exec =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "executes orc"
    [%map_open
      let input = anon (maybe ("INPUT" %: file))
      and bc = bc_flag
      and prelude = prelude_flag
      and dump = dump_flag
      and includes = includes_flag
      and exts = exts_flag
      and verbose = verbose_flag in
      let exec () =
        load_exts exts;
        compile_input_and_deps prelude includes bc input
        >>= fun inter ->
        match Orcml.run inter with
        | Ok(v) -> run_loop dump (Orcml.unblock inter) v
        | Error(err) ->
          error "Runtime error:\n%s" (Error.to_string_hum err);
          exit 1 in
      fun () ->
        set_logging verbose;
        exec () |> ignore;
        Scheduler.go () |> never_returns
    ]

let load_instance path =
  match%bind file_contents path with
  | Error(err) ->
    error "Can't read state file:%s" (Error.to_string_hum err);
    exit 1
  | Ok(bc_raw) ->
    let (_, packed) = Msgpck.String.read bc_raw in
    match Orcml.Serializer.load_instance packed with
    | Error(err) ->
      error "Can't parse state file:%s" (error_to_string_hum err);
      exit 1;
    | Ok(instance) -> return instance

let parse_value v =
  match Orcml.parse_value v with
  | Ok(v') -> return v'
  | Error(err) ->
    error "Can't parse value:%s" (error_to_string_hum err);
    exit 1

let unblock =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "continue execution of serialized orc program"
    [%map_open
      let input = anon (maybe ("INPUT" %: file))
      and bc = bc_flag
      and prelude = prelude_flag
      and dump = dump_flag
      and load = flag "-load" (required file) ~doc:"Serialized state"
      and id = flag "-id" (required int)  ~doc:"Coeffect's id"
      and value = flag "-value" (required string) ~doc:"Coeffect's value"
      and includes = includes_flag
      and verbose = verbose_flag in
      let exec () =
        compile_input_and_deps prelude includes bc input
        >>= fun inter ->
        load_instance load >>= fun instance ->
        parse_value value >>= fun value' ->
        match Orcml.unblock inter instance id value' with
        | Ok(v) -> run_loop dump (Orcml.unblock inter) v
        | Error(err) ->
          error "Runtime error:\n%s" (Error.to_string_hum err);
          exit 1 in
      fun () ->
        set_logging verbose;
        exec () |> ignore;
        Scheduler.go () |> never_returns
    ]

let tests_server =
  let module Serializer = Orcml.Testkit.Serializer in
  let module Protocol = Orcml_testkit.Protocol in
  let write v = Protocol.write (Lazy.force Writer.stdout) v in
  let write_result r =
    write (Serializer.dump_res r) in
  let rec server inter state =
    let input = (Lazy.force Reader.stdin) in
    Protocol.read_msg_or_exit ~code:0 input >>= fun msg ->
    match Serializer.load_msg msg
          |> Result.map_error ~f:(fun _ -> "Protocol error")
          |> Result.ok_or_failwith with
    | Execute(bc) ->
      let inter = Orcml.inter bc
                  |> Result.map_error ~f:error_to_string_hum
                  |> Result.ok_or_failwith in
      handle_res (Some inter) (Orcml.run inter)
    | Continue(id, v) ->
      let res = Orcml.unblock (Option.value_exn inter) (Option.value_exn state) id v in
      handle_res inter res
    | Benchmark(bc, iter) ->
            let inter = Orcml.inter bc
                  |> Result.map_error ~f:error_to_string_hum
                  |> Result.ok_or_failwith in
      (match Benchmark.latency1 ~style:Benchmark.Nil
               (Int64.of_int iter) Orcml.run inter with
      | [(_, [{Benchmark.wall}])] ->
        write (Serializer.dump_bench_res (wall *. 1000.0));
        server None None
      | _ -> assert false)
  and handle_res inter = function
    | Ok(({Orcml.Res.instance} as v)) ->
      write_result v;
      server inter (Some instance)
    | Error(err) ->
      error "Runtime error:\n%s" (Error.to_string_hum err);
      exit 1 in
  let open Command.Let_syntax in
  Command.basic
    ~summary: "tests-server. supports TestKit protocol"
    [%map_open
      let verbose = verbose_flag in
      fun () ->
        set_logging verbose;
        (Monitor.try_with_or_error (fun () -> server None None)
         >>= function
         | Ok(()) -> Async.return ()
         | Error(err) ->
           error "Unknown error while tests run:\n%s\n" (Error.to_string_hum err);
           exit 1)|> don't_wait_for;
        Scheduler.go () |> never_returns
    ]

let () =
  Command.group ~summary:"Orc programming language compiler and VM"
    [("compile", compile);
     ("exec", exec);
     ("unblock", unblock);
     ("tests-server", tests_server)]
  |> Command.run
