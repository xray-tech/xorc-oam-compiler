open! Core
open! Async
open! Log.Global

module Lib = Orcml_bin_lib

let set_logging verbose =
  (if verbose then set_level `Debug);
  let output = (Log.Output.stderr ()) in
  set_output [output]

let verbose_flag =
  let open Command.Param in
  flag "verbose" no_arg ~doc:"Verbose logging"

let read_file_or_stdin = function
  | None -> Reader.contents (Lazy.force Reader.stdin)
  | Some(f) -> Reader.file_contents f

let list_position l ~f =
  let rec step acc = function
    | [] -> None
    | x::_ when f x -> Some(acc)
    | _::xs -> step (acc + 1) xs in
  step 0 l

let compile_source loader include_prelude prog =
  let (module Loader : Lib.ModuleLoader) = loader in
  let prelude = if include_prelude
    then Orcml.implicit_prelude
    else [] in
  let%bind modules = Loader.all_modules () in
  match%map Lib.add_modules loader modules with
  | Error(err) -> Error(err)
  | Ok(repository) ->
    match Orcml.compile ~prelude ~repository prog with
    | Error _ as err -> err
    | Ok(bc) ->
      debug "Compiled:\n%s" (Orcml.sexp_of_bc bc |> Sexp.to_string_hum);
      Ok bc

let compile_bc prog =
  let (_, packed) = Msgpck.String.read prog in
  return (Orcml.Serializer.load packed)

let compile_input prelude loader is_byte_code prog =
  if is_byte_code
  then compile_bc prog
  else compile_source loader prelude prog

let no_prelude_flag =
  let open Command.Param in
  flag "-no-prelude" no_arg ~doc:"Do not implicity refer whole prelude"

let includes_flag =
  let open Command.Param in
  flag "-i" (listed string) ~doc:"Directories to include"

let exts_flag =
  let open Command.Param in
  flag "-ext" (listed string) ~doc:"Extensions"

let bc_flag =
  let open Command.Param in
  flag "-bc" no_arg ~doc:"Execute bytecode, not Orc source file. By default reads from stdin"

let dump_flag =
  let open Command.Param in
  flag "-dump" (optional string) ~doc:"Path to store intermediate state if any"

let error_to_string_hum (module Loader : Lib.ModuleLoader) prog = function
  | `CantLoadMod mod_ -> return (sprintf "Can't load module %s" mod_)
  | (`NoInput | `BadFormat | `UnsupportedValueAST | `UnknownFFC _ | `UnknownReferedFunction _) as other ->
    return (Orcml.error_to_string_hum other)
  | `SyntaxError ({Orcml.line; col; path}, _) | `UnboundVar ({Orcml.start = {line; col; path}}, _) as err ->
    let msg = Orcml.error_to_string_hum err in
    match (prog, path) with
    | (Some prog, "")  ->
      return ("\n" ^ Lib.annotate_code prog line col msg)
    | (None, "") -> return msg
    | (_, path) ->
      match%map Loader.load path with
      | Some(prog) -> sprintf "\nModule \"%s\":\n\n%s" path (Lib.annotate_code prog line col msg)
      | None -> sprintf "%s\n(Can't find source code for module %s)" msg path

let msgpack_format bc =
  Msgpck.String.to_string (Orcml.Serializer.dump bc)
  |> Bytes.to_string

let k_format = Orcml.Serializer.dump_k

let compile =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "produce bytecode"
    [%map_open
      let input = anon (maybe ("INPUT" %: string))
      and output = flag "-output" (optional string) ~doc:"Output path"
      and k = flag "-k" no_arg ~doc:"Output in K format"
      and includes = includes_flag
      and no_prelude = no_prelude_flag
      and verbose = verbose_flag in
      let exec () =
        let open Async.Deferred.Let_syntax in
        let%bind prog = read_file_or_stdin input in
        let loader = Lib.multiloader (Lib.static_prelude::(List.map includes ~f:Lib.fs)
|> List.rev) in
        let%bind res = compile_source loader (not no_prelude) prog  in
        (match res with
         | Error(err) ->
           let%bind message = error_to_string_hum loader (Some prog) err in
           error "Can't compile: %s" message;
           exit 1
         | Ok(bc) ->
           let bc' = if k
             then k_format bc
             else msgpack_format bc in
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
  Random.self_init ();
  let on_air = ref 0 in
  let stopped = Ivar.create () in
  let minstance = Moption.create () in
  let module V = Orcml.Value in
  let module C = Orcml.Const in
  let dump_state instance =
    match state_path with
    | Some(state_path) ->
      let msgpck = Orcml.Serializer.dump_instance instance in
      Writer.save state_path
        ~contents:(Msgpck.String.to_string msgpck |> Bytes.to_string)
    | _ -> return () in
  let coeffect_name = function
    | V.VRecord(pairs) ->
      (match List.Assoc.find pairs ~equal:String.equal "name" with
       | Some(V.VConst(C.String v)) -> Some(v, pairs)
       | _ -> None)
    | _ -> None in
  let rec handle_rwait id r =
    match List.Assoc.find_exn r ~equal:String.equal "delay" with
    | V.VConst(C.Int(v)) ->
      let timeout = Float.of_int v |> Time.Span.of_ms in
      after timeout >>= fun () ->
      on_air := !on_air - 1;
      unblock' id (V.VConst C.Signal)
    | v ->
      error "Bad type for timeout value: %s" (Orcml.Value.to_string v);
      return ()
  and handle_println id r =
    let v = List.Assoc.find_exn r ~equal:String.equal "value" in
    info "Println: %s" (Orcml.Value.to_string v);
    on_air := !on_air - 1;
    unblock' id (V.VConst C.Signal)
  and handle_random id r =
    match List.Assoc.find r ~equal:String.equal "bound" with
    | Some(V.VConst(C.Int(i))) -> let x = Random.int i in
      on_air := !on_air - 1;
      unblock' id (V.VConst(C.Int x))
    | Some(V.VConst(C.Float(f))) -> let x = Random.float f in
      on_air := !on_air - 1;
      unblock' id (V.VConst(C.Float x))
    | None | Some(_) -> let x = Random.bits () in
      on_air := !on_air - 1;
      unblock' id (V.VConst(C.Int x))
  and handle_clock id r =
    match List.Assoc.find r ~equal:String.equal "kind" with
    | Some(V.VConst(C.String v)) ->
      (match v with
      | "MONOTONIC" ->
        let t =  Int64.to_float(Mtime_clock.now_ns ()) /. Mtime.s_to_ns in
        on_air := !on_air - 1;
        unblock' id (V.VConst(C.Float t))
      | "REAL-TIME" | "REALTIME" ->
        on_air := !on_air -1;
        unblock' id (V.VConst(C.Float (Unix.gettimeofday ())))
      | v ->
        error "%s: unknown clock kind" v;
        return ())
    | _ ->
      error "Bad type for clock kind";
      return ()
  and handlers = [
    ("rwait", handle_rwait);
    ("println", handle_println);
    ("random", handle_random);
    ("clock", handle_clock)]
  and coeffect_handler v =
    let open Option.Let_syntax in
    let%bind (kind, pairs) = coeffect_name v in
    let%map handler = List.Assoc.find handlers ~equal:String.equal kind in
    (handler, pairs)
  and tick {Orcml.Res.values; coeffects; instance} =
    Moption.set_some minstance instance;
    List.iter values ~f:(fun v ->
        info "Value: %s" (Orcml.Value.to_string v));
    on_air := !on_air + List.length coeffects;
    List.iter coeffects ~f:(fun (id, v) ->
        match coeffect_handler v with
        | Some(handler, pairs) ->
          handler id pairs |> don't_wait_for
        | None ->
          on_air := !on_air - 1;
          info "Coeffect: %i -> %s" id (Orcml.Value.to_string v));
    match (Orcml.is_running instance, !on_air) with
    | (false, _) ->
      Ivar.fill_if_empty stopped true; return ()
    | (true, 0) ->
      dump_state instance >>| fun () ->
      Ivar.fill_if_empty stopped true
    | (true, _) -> return ()
  and unblock' id v =
    let res = unblock (Moption.get_some_exn minstance) id v in
    tick res in
  tick res |> don't_wait_for;
  let%bind res = Ivar.read stopped in
  if res
  then exit 0
  else exit 1

let compile_input_and_deps prelude loader bc prog =
  let prog_for_errors = if bc then None else Some prog in
  compile_input prelude loader bc prog >>= fun res ->
  (match res with
   | Error(err) ->
     let%bind message = error_to_string_hum loader prog_for_errors err in
     error "Can't compile: %s" message;
     exit 1
   | Ok(bc) ->
     match Orcml.inter bc with
     | Error(err) ->
       let%bind message = error_to_string_hum loader prog_for_errors err in
       error "Can't make runner: %s" message;
       exit 1
     | Ok(inter) ->
       return inter)

let load_exts exts =
  List.iter exts ~f:(fun ext ->
      try Dynlink.loadfile ext with
      | Dynlink.Error err -> error "Ext %s load error: %s" ext (Dynlink.error_message err))

let exec =
  Signal.handle Signal.terminating ~f:(fun _ ->
      shutdown 0);
  let open Command.Let_syntax in
  Command.basic
    ~summary: "executes orc"
    [%map_open
      let input = anon (maybe ("INPUT" %: string))
      and bc = bc_flag
      and no_prelude = no_prelude_flag
      and dump = dump_flag
      and includes = includes_flag
      and exts = exts_flag
      and debugger = flag "debugger" no_arg ~doc:"Run with debugger"
      and verbose = verbose_flag in
      let exec () =
        let open Deferred.Let_syntax in
        load_exts exts;
        let%bind prog = read_file_or_stdin input in
        let loader = Lib.multiloader (Lib.static_prelude::(List.rev_map includes ~f:Lib.fs)
                                  |> List.rev) in
        compile_input_and_deps (not no_prelude) loader bc prog
        >>= fun inter ->
        if debugger
        then
          let%bind () = Debugger.run loader prog inter in
          exit 0
        else
          let res = Orcml.run inter in
          run_loop dump (Orcml.unblock inter) res in
      fun () ->
        set_logging verbose;
        exec () |> ignore;
        Scheduler.go () |> never_returns
    ]

let file_contents path =
  let%map res = Monitor.try_with (fun () -> Reader.file_contents path ) in
  Result.map_error res ~f:(fun e -> Error.of_exn e )

let load_instance path =
  match%bind file_contents path with
  | Error(err) ->
    error "Can't read state file:%s" (Error.to_string_hum err);
    exit 1
  | Ok(bc_raw) ->
    let (_, packed) = Msgpck.String.read bc_raw in
    match Orcml.Serializer.load_instance packed with
    | Error(err) ->
      error "Can't parse state file:%s" (Orcml.error_to_string_hum err);
      exit 1;
    | Ok(instance) -> return instance

let parse_value v =
  match Orcml.parse_value v with
  | Ok(v') -> return v'
  | Error(err) ->
    error "Can't parse value:%s" (Orcml.error_to_string_hum err);
    exit 1

let unblock =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "continue execution of serialized orc program"
    [%map_open
      let input = anon (maybe ("INPUT" %: string))
      and bc = bc_flag
      and no_prelude = no_prelude_flag
      and dump = dump_flag
      and load = flag "-load" (required string) ~doc:"Serialized state"
      and id = flag "-id" (required int)  ~doc:"Coeffect's id"
      and value = flag "-value" (required string) ~doc:"Coeffect's value"
      and includes = includes_flag
      and verbose = verbose_flag in
      let exec () =
        let open Deferred.Let_syntax in
        let%bind prog = read_file_or_stdin input in
        let loader = Lib.multiloader (List.map includes ~f:Lib.fs) in
        compile_input_and_deps (not no_prelude) loader bc prog
        >>= fun inter ->
        load_instance load >>= fun instance ->
        parse_value value >>= fun value' ->
        let res = Orcml.unblock inter instance id value' in
        run_loop dump (Orcml.unblock inter) res in
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
                  |> Result.map_error ~f:Orcml.error_to_string_hum
                  |> Result.ok_or_failwith in
      handle_res (Some inter) (Orcml.run inter)
    | Continue(id, v) ->
      let instance = state
                     |> Option.value_exn
                     |> Orcml.Serializer.load_instance
                     |> Result.map_error ~f:Orcml.error_to_string_hum
                     |> Result.ok_or_failwith in
      let res = Orcml.unblock (Option.value_exn inter) instance id v in
      handle_res inter res
    | Benchmark(bc, iter) ->
      let inter = Orcml.inter bc
                  |> Result.map_error ~f:Orcml.error_to_string_hum
                  |> Result.ok_or_failwith in
      (match Benchmark.latency1 ~style:Benchmark.Nil
               (Int64.of_int iter) Orcml.run inter with
      | [(_, [{Benchmark.wall}])] ->
        write (Serializer.dump_bench_res (wall *. 1000.0));
        server None None
      | _ -> assert false)
  and handle_res inter ({Orcml.Res.instance} as v) =
    write_result v;
    server inter (Some (Orcml.Serializer.dump_instance instance)) in
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
           error "Unknown error while tests run:\n%s\n"
             (Error.to_string_hum err);
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
