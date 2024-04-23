open StdLabels

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

let read_file path = In_channel.with_open_bin path In_channel.input_all

type post = { yaml : Yaml.value; basename : string; tags : string list }

let load_post path =
  let contents = read_file path in
  let yaml = Yaml.of_string_exn contents in
  let basename = Filename.basename path in
  let tags =
    Yaml.Util.find_exn "tags" yaml
    |> Option.get |> Yaml.Util.to_string_exn
    |> String.split_on_char ~sep:','
    |> List.map ~f:String.trim
  in
  { yaml; basename; tags }

let load_all_posts path =
  Sys.readdir path |> Array.to_list
  |> List.sort ~cmp:String.compare
  |> List.map ~f:(fun base -> load_post (Filename.concat path base))

let run ~input ~output =
  let ops = match output with None -> dry_run | Some root -> real_ops ~root in
  let posts = load_all_posts (Filename.concat input "posts") in
  let all_tags =
    posts
    |> List.concat_map ~f:(fun p -> p.tags)
    |> List.sort_uniq ~cmp:String.compare
  in
  ops.mkdir ".";
  ops.mkdir "posts";
  List.iter posts ~f:(fun post ->
      let path = Filename.concat "posts" post.basename in
      let contents = "\n" in
      ops.create_file ~path ~contents);
  List.iter all_tags ~f:(fun tag ->
      let path = Filename.concat "tags" tag in
      let contents = "\n" in
      ops.create_file ~path ~contents)

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
