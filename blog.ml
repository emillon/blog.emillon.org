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

type post = { basename : string; tags : string list; title : string }

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
  { basename; tags; title }

type feed_config = {
  title : string;
  description : string;
  author_name : string;
  author_email : string;
  root : Uri.t;
}

let load_all_posts path =
  Sys.readdir path |> Array.to_list
  |> List.filter ~f:(fun s -> Filename.check_suffix s ".mdwn")
  |> List.sort ~cmp:String.compare
  |> List.map ~f:(fun base -> load_post (Filename.concat path base))

let post_permalink _post = "permalink"
let post_rss_description _post = "description"

let post_pubdate post =
  Scanf.sscanf post.basename "%d-%d-%d" (fun y m d -> (y, m, d))

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
  let pa = Ptime.of_date a |> Option.get in
  let pb = Ptime.of_date b |> Option.get in
  Ptime.compare pa pb

let date_to_rss_string date =
  let ptime = Ptime.of_date date |> Option.get in
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
       Filename.concat path "rss.xml")
  in
  let items = List.map posts ~f:(post_to_rss_item ~config) in
  let oldest_date =
    List.map posts ~f:post_pubdate |> List.sort ~cmp:compare_date |> List.hd
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
  Format.asprintf "%a" (pp ()) root

let feed_config =
  {
    title = "Enter the void *";
    description = "Yet another random hacker";
    author_name = "Etienne Millon";
    author_email = "me@emillon.org";
    root = Uri.of_string "http://blog.emillon.org";
  }

let run ~input ~output =
  let ops = match output with None -> dry_run | Some root -> real_ops ~root in
  let posts = load_all_posts (Filename.concat input "posts") in
  let all_tags =
    posts
    |> List.concat_map ~f:(fun p -> p.tags)
    |> List.sort_uniq ~cmp:String.compare
  in
  let rss_feed = rss_feed posts feed_config in
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
