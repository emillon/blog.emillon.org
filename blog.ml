module Cmdliner_let_syntax = struct
  let ( let+ ) x f =
    let open Cmdliner.Term in
    const f $ x

  let pair x y = (x, y)

  let ( and+ ) x y =
    let open Cmdliner.Term in
    const pair $ x $ y
end

type ops = {
  mkdir : string -> unit;
  create_file : path:string -> contents:string -> unit;
}

let real_ops ~root =
  let mkdir path = Sys.mkdir (Filename.concat root path) 0o755 in
  let create_file ~path ~contents =
    Out_channel.with_open_bin (Filename.concat root path) (fun oc ->
        Out_channel.output_string oc contents)
  in
  { mkdir; create_file }

let dry_run =
  let mkdir path = Printf.printf "mkdir %S\n" path in
  let create_file ~path ~contents =
    let hash = Digest.string contents |> Digest.to_hex in
    Printf.printf "create_file %S (hash: %s)\n" path hash
  in
  { mkdir; create_file }

let run ~input:_ ~output =
  let ops = match output with None -> dry_run | Some root -> real_ops ~root in
  ops.mkdir ".";
  ops.create_file ~path:"test" ~contents:"hello\n"

let info = Cmdliner.Cmd.info "blog"

let term =
  let open Cmdliner_let_syntax in
  let+ input =
    let open Cmdliner.Arg in
    required & opt (some string) None & info [ "i" ]
  and+ output =
    let open Cmdliner.Arg in
    value & opt (some string) None & info [ "o" ]
  in
  run ~input ~output

let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
