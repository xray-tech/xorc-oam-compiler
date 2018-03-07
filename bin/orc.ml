open! Core
open! Async
open! Log.Global

module List = struct
  include List
  let safe_sub l ~pos ~len =
    let llen = List.length l in
    List.sub l ~pos:(Int.min pos (llen - 1)) ~len:(Int.min len (llen - pos))
end

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

module type ModuleLoader = sig
  val load : string -> string option Deferred.t
  val all_modules : unit -> string list Deferred.t
end

let list_position l ~f =
  let rec step acc = function
    | [] -> None
    | x::_ when f x -> Some(acc)
    | _::xs -> step (acc + 1) xs in
  step 0 l

let compile_modules (module Loader : ModuleLoader) modules =
  let repository = Orcml.Repository.create () in
  let rec step = function
    | [] -> return (Ok repository)
    | mod_::xs ->
      match%bind Loader.load mod_ with
      | None -> Error(`CantLoadMod mod_) |> return
      | Some(code) ->
        match Orcml.compile_module ~repository ~name:mod_ code with
        | Error _ as err -> return err
        | Ok(()) -> step xs in
  step modules

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
    let load mod_ =
      let mod_' = String.Search_pattern.(replace_all (create "\\.") ~in_:mod_ ~with_:"/") ^ ".orc" in
      optional_file_contents (Filename.concat path mod_')
    (* TODO nested directories *)
    let all_modules () =
      let%map dirs = Sys.ls_dir path in
      List.filter_map dirs ~f:(fun n ->
          match Filename.split_extension n with
          | (mod_, Some("orc")) -> Some(mod_)
          | _ -> None)
  end : ModuleLoader)

let multiloader loaders =
  (module struct
    let load mod_ = Deferred.List.find_map loaders ~f:(fun (module Loader : ModuleLoader) ->
        Loader.load mod_)

    let all_modules () =
      Deferred.List.concat_map loaders ~f:(fun (module Loader : ModuleLoader) ->
          Loader.all_modules ())
  end : ModuleLoader)

let empty_loader =
  (module struct
    let load _mod = return None
    let all_modules () = return []
  end : ModuleLoader)

let implicit_prelude =
  [("core", ["abs"; "signum"; "min"; "max"; "+"; "-"; "*"; "/"; "%"; "**"; "="; "/=";
             ":>"; ">="; "<:"; "<="; "||"; "&&"; "~"; ":"; "Ift"; "Iff"; "ceil";
             "floor"; "sqrt"; "Let"; "Rwait"; "Println"]);
   ("idioms", ["curry"; "curry3"; "uncurry"; "uncurry3"; "flip"; "constant"; "defer";
               "defer2"; "ignore"; "ignore2"; "compose"; "while"; "repeat"; "fork";
               "forkMap"; "seq"; "seqMap"; "join"; "joinMap"; "alt"; "altMap"; "por";
               "pand"]);
   ("list", ["each"; "map"; "reverse"; "filter"; "head"; "tail"; "init"; "last";
             "empty"; "index"; "append"; "foldl"; "foldl1"; "foldr"; "foldr1";
             "afold"; "zipWith"; "zip"; "unzip"; "concat"; "length"; "take"; "drop";
             "member"; "merge"; "mergeBy"; "sort"; "sortBy"; "mergeUnique";
             "mergeUniqueBy"; "sortUnique"; "sortUniqueBy"; "group"; "groupBy";
             "rangeBy"; "range"; "any"; "all"; "sum"; "product"; "and"; "or";
             "minimum"; "maximum"])]

let compile_source loader include_prelude prog =
  let (module Loader : ModuleLoader) = loader in
  let prelude = if include_prelude
    then List.concat_map implicit_prelude ~f:(fun (mod_, idents) ->
        List.map idents ~f:(fun ident -> (mod_, ident)))
    else [] in
  let%bind modules = Loader.all_modules () in
  match%map compile_modules loader modules with
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

let compile_input prelude includes is_byte_code prog =
  let loader = multiloader (List.map includes ~f:fs) in
  if is_byte_code
  then compile_bc prog
  else compile_source loader prelude prog

let make_bytecode prelude includes prog =
  let loader = multiloader (List.map includes ~f:fs) in
  compile_source loader prelude prog

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

let msg_with_positions prog msg line col =
  match prog with
  | Some(prog) ->
    let context_before = 5 in
    let context_after = 3 in
    (* Including trailing newline *)
    let lines = String.split_on_chars prog ~on:['\n'] in
    let from_line = Int.max (line - context_before) 0 in
    let indexed_lines = List.mapi lines ~f:(fun i x -> (Some(i), x)) in
    let msg' = String.(make col ' ' ^ "^--- " ^ msg) in
    let line_width = List.length lines |> Int.to_string |> String.length in
    let line_format = Scanf.format_from_string ("%" ^ Int.to_string line_width ^ "d: %s") "%d%s" in
    let res = List.concat
        [List.sub indexed_lines ~pos:from_line ~len:((Int.min line context_before) + 1);
         [(None, msg')];
         List.safe_sub indexed_lines ~pos:(line + 1) ~len:context_after;]
              |> List.map ~f:(function
                  | (Some(l), x) -> sprintf line_format l x
                  | (None, x) -> sprintf "%s  %s" (String.make line_width ' ') x)
              |> String.concat ~sep:"\n" in
    "\n" ^ res
  | None -> msg

let error_to_string_hum prog = function
  | `CantLoadMod mod_ -> (sprintf "Can't load module %s" mod_)
  | `SyntaxError ((line, col), _) | `UnboundVar ((line, col), _) as err ->
    let msg = Orcml.error_to_string_hum err in
    msg_with_positions prog msg line col
  | (`NoInput | `BadFormat | `UnsupportedValueAST | `UnknownFFI _ | `UnknownReferedFunction _) as other ->
    Orcml.error_to_string_hum other

let msgpack_format bc =
  Msgpck.String.to_string (Orcml.Serializer.dump bc)
  |> Bytes.to_string

let k_format = Orcml.Serializer.dump_k

let compile =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "produce bytecode"
    [%map_open
      let input = anon (maybe ("INPUT" %: file))
      and output = flag "-output" (optional file) ~doc:"Output path"
      and k = flag "-k" no_arg ~doc:"Output in K format"
      and includes = includes_flag
      and prelude = prelude_flag
      and verbose = verbose_flag in
      let exec () =
        let open Async.Deferred.Let_syntax in
        let%bind prog = read_file_or_stdin input in
        let%bind res = make_bytecode prelude includes prog in
        (match res with
         | Error(err) ->
           error "Can't compile: %s" (error_to_string_hum (Some prog) err);
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
  let on_air = ref 0 in
  let stopped = Ivar.create () in
  let minstance = Moption.create () in
  let module V = Orcml.Value in
  let module C = Orcml.Const in
  let dump_state instance =
    match state_path with
    | Some(state_path) ->
      let msgpck = Orcml.Serializer.dump_instance instance in
      Writer.save state_path ~contents:(Msgpck.String.to_string msgpck |> Bytes.to_string)
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
  let%bind prog = read_file_or_stdin input in
  let prog_for_errors = if bc then None else Some prog in
  compile_input prelude includes bc prog >>= fun res ->
  (match res with
   | Error(err) ->
     error "Can't compile: %s" (error_to_string_hum prog_for_errors err);
     exit 1
   | Ok(bc) ->
     match Orcml.inter bc with
     | Error(err) ->
       error "Can't make runner: %s" (error_to_string_hum prog_for_errors err);
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
      and debugger = flag "debugger" no_arg ~doc:"Run with debugger"
      and verbose = verbose_flag in
      let exec () =
        load_exts exts;
        compile_input_and_deps prelude includes bc input
        >>= fun inter ->
        if debugger
        then Debugger.run inter
        else
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
      error "Can't parse state file:%s" (error_to_string_hum None err);
      exit 1;
    | Ok(instance) -> return instance

let parse_value v =
  match Orcml.parse_value v with
  | Ok(v') -> return v'
  | Error(err) ->
    error "Can't parse value:%s" (error_to_string_hum (Some v) err);
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
                  |> Result.map_error ~f:(error_to_string_hum None)
                  |> Result.ok_or_failwith in
      handle_res (Some inter) (Orcml.run inter)
    | Continue(id, v) ->
      let res = Orcml.unblock (Option.value_exn inter) (Option.value_exn state) id v in
      handle_res inter res
    | Benchmark(bc, iter) ->
      let inter = Orcml.inter bc
                  |> Result.map_error ~f:(error_to_string_hum None)
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
