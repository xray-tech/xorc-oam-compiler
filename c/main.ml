open! Base

let repository = Orcml.Repository.create ()

let orcml_run prog =
  let code = Stdio.In_channel.read_all prog in
  match Orcml.compile ~repository code with
  | Ok bc ->
    (match Orcml.inter bc with
     | Error _ -> "<error>"
     | Ok inter -> let { Orcml.Res.values } = Orcml.run inter in
       String.concat ~sep:", " (List.map ~f:Orcml.Value.to_string values))
  | Error _ -> "<error>"

let () =
  Caml.Callback.register "orcml_run" orcml_run
