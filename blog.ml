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
  let real_mkdir path = Sys.mkdir path 0o755 in
  let mkdir path =
    let full_path =
      match path with "." -> root | _ -> Filename.concat root path
    in
    real_mkdir full_path
  in
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

type post = {
  yaml : Yaml.value;
  basename : string;
  tags : string list;
  title : string;
}

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
  let title =
    Yaml.Util.find_exn "title" yaml |> Option.get |> Yaml.Util.to_string_exn
  in
  { yaml; basename; tags; title }

let load_all_posts path =
  Sys.readdir path |> Array.to_list
  |> List.filter ~f:(fun s -> Filename.check_suffix s ".mdwn")
  |> List.sort ~cmp:String.compare
  |> List.map ~f:(fun base -> load_post (Filename.concat path base))

let post_permalink _post = "permalink"
let post_rss_description _post = "description"
let post_pubdate _post = "description"

let post_to_rss_item post =
  let open Tyxml.Xml in
  node "item"
    [
      node "title" [ pcdata post.title ];
      node "link" [ pcdata (post_permalink post) ];
      node "description" [ cdata (post_rss_description post) ];
      node "pubDate" [ pcdata (post_pubdate post) ];
      node "guid" [ pcdata (post_permalink post) ];
      node "dc:creator" [ pcdata "Etienne Millon" ];
    ]

let rss_feed posts =
  let open Tyxml.Xml in
  let items = List.map posts ~f:post_to_rss_item in
  let channel =
    node "channel"
      ([
         node "title" [ pcdata "Enter the void *" ];
         node "link" [ pcdata "http://blog.emillon.org" ];
         node "description" [ cdata "Yet another random hacker" ];
         node "atom:link"
           ~a:
             [
               string_attrib "href" "http://blog.emillon.org/rss.xml";
               string_attrib "rel" "self";
               string_attrib "type" "application/rss+xml";
             ]
           [];
         node "lastBuildDate" [ pcdata "Fri, 11 Nov 2011 00:00:00 UT" ];
       ]
      @ items)
  in
  let root =
    node "rss"
      ~a:
        [
          string_attrib "version" "2.0";
          string_attrib "xmlns:atom" "http://www.w3.org/2005/Atom";
          string_attrib "xmlns:dc" "http://purl.org/dc/elements/1.1/";
        ]
      [ channel ]
  in
  Format.asprintf "%a" (Tyxml.Xml.pp ()) root

let run ~input ~output =
  let ops = match output with None -> dry_run | Some root -> real_ops ~root in
  let posts = load_all_posts (Filename.concat input "posts") in
  let all_tags =
    posts
    |> List.concat_map ~f:(fun p -> p.tags)
    |> List.sort_uniq ~cmp:String.compare
  in
  let rss_feed = rss_feed posts in
  ops.mkdir ".";
  ops.mkdir "posts";
  List.iter posts ~f:(fun post ->
      let path = Filename.concat "posts" post.basename in
      let contents = "\n" in
      ops.create_file ~path ~contents);
  ops.mkdir "tags";
  List.iter all_tags ~f:(fun tag ->
      let path = Filename.concat "tags" tag in
      let contents = "\n" in
      ops.create_file ~path ~contents);
  ops.create_file ~path:"rss.xml" ~contents:rss_feed

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
