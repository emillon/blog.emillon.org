open Base
open Stdio

module Cmdliner_let_syntax = struct
  let ( let+ ) x f =
    let open Cmdliner.Term in
    const f $ x

  let pair x y = (x, y)

  let ( and+ ) x y =
    let open Cmdliner.Term in
    const pair $ x $ y
end

let ( // ) = Stdlib.Filename.concat

type ops = {
  mkdir : string -> unit;
  create_file : path:string -> contents:string -> unit;
}

let real_ops ~root =
  let real_mkdir path = Stdlib.Sys.mkdir path 0o755 in
  let mkdir path =
    let full_path = match path with "." -> root | _ -> root // path in
    real_mkdir full_path
  in
  let create_file ~path ~contents =
    Out_channel.write_all (root // path) ~data:contents
  in
  { mkdir; create_file }

let dry_run =
  let mkdir path = printf "mkdir %S\n" path in
  let create_file ~path ~contents =
    let hash = Stdlib.Digest.string contents |> Stdlib.Digest.to_hex in
    printf "create_file %S (hash: %s)\n" path hash
  in
  { mkdir; create_file }

type post = {
  basename : string;
  tags : Set.M(String).t;
  title : string;
  author : string;
}

let load_post path =
  let contents = In_channel.read_all path in
  let yaml = Yaml.of_string_exn contents in
  let basename = Stdlib.Filename.basename path in
  let tags =
    Yaml.Util.find_exn "tags" yaml
    |> Option.value_exn |> Yaml.Util.to_string_exn |> String.split ~on:','
    |> List.map ~f:String.strip
    |> Set.of_list (module String)
  in
  let title =
    Yaml.Util.find_exn "title" yaml
    |> Option.value_exn |> Yaml.Util.to_string_exn
  in
  let author =
    Yaml.Util.find_exn "author" yaml
    |> Option.value_exn |> Yaml.Util.to_string_exn
  in
  { basename; tags; title; author }

type feed_config = {
  title : string;
  description : string;
  author_name : string;
  author_email : string;
  root : Uri.t;
}

let readdir path =
  Stdlib.Sys.readdir path |> Array.to_list
  |> List.sort ~compare:String.compare
  |> List.map ~f:(fun base -> path // base)

let load_all_posts path =
  readdir path
  |> List.filter ~f:(fun s -> Stdlib.Filename.check_suffix s ".mdwn")
  |> List.map ~f:load_post

let post_permalink _post = "permalink"
let post_rss_description _post = "description"

let post_pubdate post =
  Stdlib.Scanf.sscanf post.basename "%d-%d-%d" (fun y m d -> (y, m, d))

let weekday_to_string = function
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"
  | `Sun -> "Sun"

let month_name = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | n -> Printf.ksprintf invalid_arg "month_name: %d" n

let compare_date a b =
  let pa = Ptime.of_date a |> Option.value_exn in
  let pb = Ptime.of_date b |> Option.value_exn in
  Ptime.compare pa pb

let date_to_rss_string date =
  let ptime = Ptime.of_date date |> Option.value_exn in
  let y, m, d = date in
  let weekday = Ptime.weekday ptime in
  Printf.sprintf "%s, %d %s %d 00:00:00 UT"
    (weekday_to_string weekday)
    d (month_name m) y

let post_to_rss_item (post : post) ~config =
  let open Tyxml.Xml in
  node "item"
    [
      node "title" [ pcdata post.title ];
      node "link" [ pcdata (post_permalink post) ];
      node "description" [ cdata (post_rss_description post) ];
      node "pubDate" [ pcdata (date_to_rss_string (post_pubdate post)) ];
      node "guid" [ pcdata (post_permalink post) ];
      node "dc:creator" [ pcdata config.author_name ];
    ]

let rss_feed posts config =
  let open Tyxml.Xml in
  let rss_uri =
    Uri.with_path config.root
      (let path = Uri.path config.root in
       path // "rss.xml")
  in
  let items = List.map posts ~f:(post_to_rss_item ~config) in
  let oldest_date =
    List.map posts ~f:post_pubdate
    |> List.min_elt ~compare:compare_date
    |> Option.value_exn
  in
  let channel =
    node "channel"
      ([
         node "title" [ pcdata config.title ];
         node "link" [ pcdata (Uri.to_string config.root) ];
         node "description" [ cdata config.description ];
         node "atom:link"
           ~a:
             [
               string_attrib "href" (Uri.to_string rss_uri);
               string_attrib "rel" "self";
               string_attrib "type" "application/rss+xml";
             ]
           [];
         node "lastBuildDate" [ pcdata (date_to_rss_string oldest_date) ];
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
  Stdlib.Format.asprintf "%a" (pp ()) root

let feed_config =
  {
    title = "Enter the void *";
    description = "Yet another random hacker";
    author_name = "Etienne Millon";
    author_email = "me@emillon.org";
    root = Uri.of_string "http://blog.emillon.org";
  }

let rec copy_files ops input_path =
  readdir input_path
  |> List.iter ~f:(fun path ->
         let out_path = String.chop_prefix_exn ~prefix:"static/" path in
         let s = Unix.stat path in
         match s.st_kind with
         | S_REG ->
             ops.create_file ~path:out_path ~contents:(In_channel.read_all path)
         | S_DIR ->
             ops.mkdir out_path;
             copy_files ops path
         | _ -> assert false)

let with_string_formatter f =
  let buf = Buffer.create 0 in
  let fmt = Stdlib.Format.formatter_of_buffer buf in
  f fmt;
  Buffer.contents buf

let run ~input ~output =
  let ops = match output with None -> dry_run | Some root -> real_ops ~root in
  let posts = load_all_posts (input // "posts") in
  let all_tags =
    List.map posts ~f:(fun p -> p.tags) |> Set.union_list (module String)
  in
  let rss_feed = rss_feed posts feed_config in
  ops.mkdir ".";
  ops.mkdir "posts";
  List.iter posts ~f:(fun post ->
      let path = "posts" // post.basename in
      let env =
        Map.of_alist_exn
          (module String)
          [
            ("title", post.title);
            ("author", post.author);
            ("date", date_to_rss_string (post_pubdate post));
            ("prettytags", String.concat ~sep:", " (Set.to_list post.tags));
            ("body", "BODY");
          ]
      in
      let contents =
        with_string_formatter (fun fmt ->
            Template.process env ~input:"templates/post.html" ~output:fmt)
      in
      ops.create_file ~path ~contents);
  ops.mkdir "tags";
  Set.iter all_tags ~f:(fun tag ->
      let path = "tags" // tag in
      let contents = "\n" in
      ops.create_file ~path ~contents);
  ops.create_file ~path:"rss.xml" ~contents:rss_feed;
  copy_files ops "static"

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
