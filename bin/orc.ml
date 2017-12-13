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

let imports_linker imports_mapping fun_ =
  let equal a b = Int.equal 0 (Orcml.Fun.compare a b) in
  (match List.Assoc.find imports_mapping ~equal fun_  with
   | Some(pointer) -> Ok(pointer)
   | None -> Or_error.error_s ([%sexp_of: string * Sexp.t] ("Unbound dependency", Orcml.Fun.sexp_of_t fun_)))

let linker imports_mapping imports p =
  match List.Assoc.find imports ~equal:Int.equal p with
  | None ->
    Ok(p)
  | Some(x) ->
    imports_linker imports_mapping x


let link_namespaces compiled =
  with_return (fun r ->
      let change_pointers shift imports_mapping imports repo =
        let size = List.length repo in
        let linker p = match List.Assoc.find imports ~equal:Int.equal p with
          (* it's local fun, so we add shift to it and do not forget that result
             code is reversed *)
          | None ->
            Ok(shift + (size - (Int.abs p)))
          (* it's pointer to external fun, we should already have it *)
          | Some(x) -> imports_linker imports_mapping x in
        Orcml.link repo linker in
      let fold_compiled (imports_mapping, all_code) (ns, (imports, repo)) =
        let shift = List.length all_code in
        match change_pointers shift imports_mapping imports repo with
        | Error(err) -> r.return (Error err)
        | Ok(repo') ->
          let m = List.mapi (List.rev repo) ~f:(fun i (name, _) -> (Orcml.Fun.{ns; name}, shift + i)) in
          (m @ imports_mapping, repo' @ all_code) in
      let (global_mapping, code) =
        List.fold (List.rev compiled) ~init:([], []) ~f:fold_compiled in
      Ok(Orcml.finalize (List.rev code), global_mapping))

let compile_namespaces (module NSLoader : NSLoader) init_queue =
  let compiled = ref [] in
  let is_already_compiled ns =
    List.find !compiled ~f:(fun (ns', _) -> String.equal ns ns') |> Option.is_some in
  let analyze code =
    let open Result.Let_syntax in
    let%bind parsed = Orcml.parse_ns code in
    debug "Parsed NS:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum);
    Orcml.translate parsed in
  let rec compile_loop = function
    | [] ->
      Ok() |> return
    | ns::queue when is_already_compiled ns ->
      compile_loop queue
    | ns::queue ->
      match%bind NSLoader.load ns with
      | None -> Error(Error.createf "Can't load %s" ns) |> return
      | Some(code) ->
        analyze code |> function
        | Error(err) -> Error(err) |> return
        | Ok((deps, e)) ->
          compile_loop deps >>=? fun () ->
          match Orcml.compile_ns e with
          | Error(err) -> Error(err) |> return
          | Ok(res) ->
            compiled := (ns, res)::!compiled;
            compile_loop queue in
  compile_loop init_queue >>=? (fun () ->
      return (link_namespaces !compiled))

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
      let ns' = String.Search_pattern.(replace_all (create "\.") ~in_:ns ~with_:"/") ^ ".orc" in
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
  "refer from core (abs, signum, min, max)
   refer from idioms (curry, curry3, uncurry, uncurry3, flip, constant, defer, defer2,
     ignore, ignore2, compose, while, repeat, fork, forkMap, seq, seqMap, join,
     joinMap, alt, altMap, por, pand)
   refer from list (each, map, reverse, filter, head, tail, init, last, empty, index,
     append, foldl, foldl1, foldr, foldr1, afold, zipWith, zip, unzip, concat,
     length, take, drop, member, merge, mergeBy, sort, sortBy, mergeUnique,
     mergeUniqueBy, sortUnique, sortUniqueBy, group, groupBy, rangeBy, range,
     any, all, sum, product, and, or, minimum, maximum) "

let compile_source loader prelude prog =
  let prog' = if prelude
    then implicit_prelude ^ prog
    else prog in
  let analyzed =
    let open Result.Let_syntax in
    let%bind parsed = Orcml.parse prog' in
    debug "Parsed:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum);
    Orcml.translate parsed in
  match analyzed with
  | Error(err) -> Error(err) |> return
  | Ok((deps, ir)) ->
    debug "Translated:\n%s" (Orcml.sexp_of_ir1 ir |> Sexp.to_string_hum);
    compile_namespaces loader deps
    >>=? fun (deps, imports_mapping) ->
    match Result.Let_syntax.(
        let%bind (imports, repo) = Orcml.compile ir in
        debug "Before linking:\n%s" (Orcml.sexp_of_repo repo |> Sexp.to_string_hum);
        let linker = linker imports_mapping imports in
        let%map repo' = Orcml.link repo linker in
        repo') with
    | Error(err) -> Error err |> return
    | Ok(repo') ->
      Ok(imports_mapping, deps, Orcml.finalize repo') |> return

let imports_to_deps imports =
  let rec f acc = function
    | [] -> String.Set.to_list acc
    | (_, {Orcml.Fun.ns})::xs -> f (String.Set.add acc ns) xs in
  f String.Set.empty imports

let compile_bc loader prog =
  let (_, packed) = Msgpck.String.read prog in
  match Orcml.Serializer.imports packed with
  | Error(err) -> return(Error err)
  | Ok(imports) ->
    let deps = imports_to_deps imports in
    compile_namespaces loader deps
    >>=? fun (deps, imports_mapping) ->
    let linker = linker imports_mapping imports in
    let res =
      Orcml.Serializer.load ~linker packed
      |> Result.map ~f:(fun bc -> (imports_mapping, deps, bc)) in
    return res

let compile_input prelude includes is_byte_code input =
  let loader = multiloader (List.map includes ~f:fs) in
  let%bind prog = read_file_or_stdin input in
  if is_byte_code
  then compile_bc loader prog
  else compile_source loader prelude prog

let make_bytecode prelude ns input =
  let%map prog = read_file_or_stdin input in
  let prog' = if prelude
    then implicit_prelude ^ prog
    else prog in
  let (parser, compiler) =
    if ns
    then (Orcml.parse_ns, Orcml.compile_ns)
    else (Orcml.parse, Orcml.compile) in
  let analyzed =
    let open Result.Let_syntax in
    let%bind parsed = parser prog' in
    debug "Parsed:\n%s" (Orcml.sexp_of_ast parsed |> Sexp.to_string_hum);
    Orcml.translate parsed in
  match analyzed with
  | Error(err) -> Error(err)
  | Ok((deps, ir)) ->
    debug "Translated:\n%s" (Orcml.sexp_of_ir1 ir |> Sexp.to_string_hum);
    info "Dependencies: %s" ([%sexp_of: string list] deps |> Sexp.to_string_hum);
    let open Result.Let_syntax in
    let%map (imports, repo) = compiler ir in
    Orcml.Serializer.dump ~imports (Orcml.finalize repo)

let prelude_flag =
  let open Command.Param in
  flag "-prelude" no_arg ~doc:"Implicity refer whole prelude"

let includes_flag =
  let open Command.Param in
  flag "-i" (listed file) ~doc:"Directories to include"

let bc_flag =
  let open Command.Param in
  flag "-bc" no_arg ~doc:"Execute bytecode, not Orc source file. By default reads from stdin"

let dump_flag =
  let open Command.Param in
  flag "-dump" (optional file) ~doc:"Path to store intermediate state if any"

let compile =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "produce bytecode"
    [%map_open
      let input = anon (maybe ("INPUT" %: file))
      and output = flag "-output" (optional file) ~doc:"Output path"
      and ns = flag "-ns" no_arg ~doc:"Compile namespace"
      and prelude = prelude_flag
      and verbose = verbose_flag in
      let exec () =
        make_bytecode prelude ns input >>= fun res ->
        (match res with
         | Error(err) ->
           error "Can't compile: %s" (Error.to_string_hum err);
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

let generate_result mapping state_path {Orcml.Res.values; coeffects; instance} =
  List.iter values ~f:(fun v ->
      info "Value: %s" (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum));
  List.iter coeffects ~f:(fun (id, v) ->
      info "Coeffect: %i -> %s" id (Orcml.Value.sexp_of_t v |> Sexp.to_string_hum));
  (match (Orcml.is_running instance, state_path) with
   | (true, Some(state_path)) ->
     let msgpck = Orcml.Serializer.dump_instance ~mapping instance in
     Writer.save state_path ~contents:(Msgpck.String.to_string msgpck)
   | _ -> return ())
  >>= fun () -> exit 0

let compile_input_and_deps prelude includes bc input  =
  compile_input prelude includes bc input >>= fun res ->
  (match res with
   | Error(err) ->
     error "Can't compile: %s" (Error.to_string_hum err);
     exit 1
   | Ok((imports_mapping, dependencies, compiled)) ->
     debug "Compiled:\n%s\nDeps:\n%s"
       (Orcml.sexp_of_bc compiled |> Sexp.to_string_hum)
       (Orcml.sexp_of_bc dependencies |> Sexp.to_string_hum);
     return (imports_mapping, dependencies, compiled))

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
      and verbose = verbose_flag in
      let exec () =
        compile_input_and_deps prelude includes bc input
        >>= fun (imports_mapping, dependencies, compiled) ->
        match Orcml.run ~dependencies compiled with
        | Ok(v) -> generate_result imports_mapping dump v
        | Error(err) ->
          error "Runtime error:\n%s" (Error.to_string_hum err);
          exit 1 in
      fun () ->
        set_logging verbose;
        exec () |> ignore;
        Scheduler.go () |> never_returns
    ]

let load_instance imports_mapping path =
  match%bind file_contents path with
  | Error(err) ->
    error "Can't read state file:%s" (Error.to_string_hum err);
    exit 1
  | Ok(bc_raw) ->
    let (_, packed) = Msgpck.String.read bc_raw in
    match Orcml.Serializer.imports packed with
    | Error(err) ->
      error "Can't parse state file:%s" (Error.to_string_hum err);
      exit 1;
    | Ok(imports) ->
      let linker p =
        match List.Assoc.find imports ~equal:Int.equal p with
        | None -> Ok(p)
        | Some(fun_) ->
          imports_linker imports_mapping fun_ in
      match Orcml.Serializer.load_instance ~linker packed with
      | Error(err) ->
        error "Can't parse state file:%s" (Error.to_string_hum err);
        exit 1;
      | Ok(instance) -> return instance

let parse_value v =
  match Orcml.parse_value v with
  | Ok(v') -> return v'
  | Error(err) ->
    error "Can't parse value:%s" (Error.to_string_hum err);
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
        >>= fun (imports_mapping, dependencies, compiled) ->
        load_instance imports_mapping load >>= fun instance ->
        parse_value value >>= fun value' ->
        match Orcml.unblock ~dependencies compiled instance id value' with
        | Ok(v) -> generate_result imports_mapping dump v
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
  let rec server code state =
    let input = (Lazy.force Reader.stdin) in
    Protocol.read_msg_or_exit ~code:0 input >>= fun msg ->
    match Serializer.load_msg msg |> Or_error.ok_exn with
    | Execute(bc) ->
      handle_res (Some bc) (Orcml.run bc)
    | Continue(id, v) ->
      let res = Orcml.unblock (Option.value_exn code) (Option.value_exn state) id v in
      handle_res code res
    | Benchmark(bc, iter) ->
      (match Benchmark.latency1 ~style:Benchmark.Nil
               (Int64.of_int iter) Orcml.run bc with
      | [(_, [{Benchmark.wall}])] ->
        write (Serializer.dump_bench_res (wall *. 1000.0));
        server None None
      | _ -> assert false)
  and handle_res code = function
    | Ok(({Orcml.Res.instance} as v)) ->
      write_result v;
      server code (Some instance)
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
