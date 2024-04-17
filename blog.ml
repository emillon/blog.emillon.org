module Cmdliner_let_syntax = struct
  let ( let+ ) x f =
    let open Cmdliner.Term in
    const f $ x
end

let info = Cmdliner.Cmd.info "blog"

let term =
  let open Cmdliner_let_syntax in
  let+ output =
    let open Cmdliner.Arg in
    required & opt (some string) None & info [ "o" ]
  in
  Sys.mkdir output 0o755;
  Out_channel.with_open_bin (Filename.concat output "test") (fun oc ->
      Out_channel.output_string oc "hello\n")

let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
