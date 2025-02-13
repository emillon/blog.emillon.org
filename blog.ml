open Base
open Stdio

let ( // ) = Stdlib.Filename.concat

let with_string_formatter f =
  let buf = Buffer.create 0 in
  let fmt = Stdlib.Format.formatter_of_buffer buf in
  f fmt;
  Buffer.contents buf

let render_template input l =
  let env = Map.of_alist_exn (module String) l in
  with_string_formatter (fun fmt -> Template.process env ~input ~output:fmt)

let render_default ~title ~body =
  render_template "templates/default.html" [ ("title", title); ("body", body) ]

let readdir path =
  Stdlib.Sys.readdir path |> Array.to_list
  |> List.sort ~compare:String.compare
  |> List.map ~f:(fun base -> path // base)

let directory_exists path =
  Stdlib.Sys.file_exists path && Stdlib.Sys.is_directory path

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

let date_to_rss_string date =
  let ptime = Ptime.of_date date |> Option.value_exn in
  let y, m, d = date in
  let weekday = Ptime.weekday ptime in
  Printf.sprintf "%s, %d %s %d 00:00:00 UT"
    (weekday_to_string weekday)
    d (month_name m) y

let date_to_short_string date =
  let y, m, d = date in
  Printf.sprintf "%d %s %d" d (month_name m) y

module Ops = struct
  type t = {
    mkdir : string -> unit;
    create_file : path:string -> contents:string -> unit;
  }

  let mkdir t = t.mkdir
  let create_file t = t.create_file

  let rec copy_files ops input_path =
    readdir input_path
    |> List.iter ~f:(fun path ->
           let out_path = String.chop_prefix_exn ~prefix:"static/" path in
           let s = Unix.stat path in
           match s.st_kind with
           | S_REG ->
               create_file ops ~path:out_path
                 ~contents:(In_channel.read_all path)
           | S_DIR ->
               mkdir ops out_path;
               copy_files ops path
           | _ -> assert false)

  let real ~root =
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
end

type feed_config = {
  title : string;
  description : string;
  author_name : string;
  author_email : string;
  root : Uri.t;
}

module Post = struct
  type t = {
    basename : string;
    tags : Set.M(String).t;
    title : string;
    author : string;
    body : Cmarkit.Doc.t;
  }

  let load path =
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
    let body = Cmarkit.Doc.of_string contents in
    { basename; tags; title; author; body }

  let load_all path =
    readdir path
    |> List.filter ~f:(fun s ->
           Stdlib.Filename.check_suffix s ".mdwn"
           || Stdlib.Filename.check_suffix s ".lhs")
    |> List.map ~f:load

  let pub_date post =
    Stdlib.Scanf.sscanf post.basename "%d-%d-%d" (fun y m d -> (y, m, d))

  (* TODO *)
  let permalink _post = "permalink"

  (* TODO *)
  let rss_description _post = "description"

  let to_rss_item post ~config =
    let open Tyxml.Xml in
    node "item"
      [
        node "title" [ pcdata post.title ];
        node "link" [ pcdata (permalink post) ];
        node "description" [ cdata (rss_description post) ];
        node "pubDate" [ pcdata (date_to_rss_string (pub_date post)) ];
        node "guid" [ pcdata (permalink post) ];
        node "dc:creator" [ pcdata config.author_name ];
      ]

  let create_assets ops path =
    Ops.mkdir ops path;
    List.iter (readdir path) ~f:(fun path ->
        Ops.create_file ops ~path ~contents:(In_channel.read_all path))

  let render post =
    let title = post.title in
    let body =
      render_template "templates/post.html"
        [
          ("title", title);
          ("author", post.author);
          ("date", date_to_short_string (pub_date post));
          ("prettytags", String.concat ~sep:", " (Set.to_list post.tags));
          ("body", Cmarkit_html.of_doc ~safe:true post.body);
        ]
    in
    render_default ~title ~body

  let create ops post =
    let chopped = "posts" // post.basename |> Stdlib.Filename.chop_extension in
    let path = chopped ^ ".html" in
    if directory_exists chopped then create_assets ops chopped;
    let contents = render post in
    Ops.create_file ops ~path ~contents
end

let compare_date a b =
  let pa = Ptime.of_date a |> Option.value_exn in
  let pb = Ptime.of_date b |> Option.value_exn in
  Ptime.compare pa pb

let rss_feed posts config =
  let open Tyxml.Xml in
  let rss_uri =
    Uri.with_path config.root
      (let path = Uri.path config.root in
       path // "rss.xml")
  in
  let items = List.map posts ~f:(Post.to_rss_item ~config) in
  let oldest_date =
    List.map posts ~f:Post.pub_date
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

let render_post_items posts =
  with_string_formatter (fun fmt ->
      List.iter posts ~f:(fun post ->
          let env =
            Map.of_alist_exn
              (module String)
              [
                ("date", date_to_rss_string (Post.pub_date post));
                ("url", Post.permalink post);
                ("title", post.title);
              ]
          in
          Template.process env ~input:"templates/postitem.html" ~output:fmt))

let render_post_list posts ~title ~feed =
  let body =
    render_template "templates/posts.html"
      [
        ("title", title);
        ("posts", render_post_items posts);
        ("feed", Uri.to_string feed);
      ]
  in
  render_default ~body ~title

module Tag = struct
  let feed config tag =
    Uri.with_path config.root
      (let path = Uri.path config.root in
       path // "tag" // Printf.sprintf "%s.xml" tag)

  let href tag = "tags" // Printf.sprintf "%s.html" tag

  let create_html ops feed_config tag posts =
    let path = href tag in
    let contents =
      render_post_list posts
        ~title:(Printf.sprintf "Posts tagged %S" tag)
        ~feed:(feed feed_config tag)
    in
    Ops.create_file ops ~path ~contents

  let create_feed ops feed_config tag posts =
    let path = "feeds" // Printf.sprintf "%s.xml" tag in
    let contents = rss_feed posts feed_config in
    Ops.create_file ops ~path ~contents
end

let feed_config =
  {
    title = "Enter the void *";
    description = "Yet another random hacker";
    author_name = "Etienne Millon";
    author_email = "me@emillon.org";
    root = Uri.of_string "http://blog.emillon.org";
  }

module Templates = struct
  (* TODO: inline *)
  let render = render_template

  let index posts ~tags =
    let tag_cloud =
      match tags with
      | None -> ""
      | Some tag_map ->
          let min_size = 70. in
          let max_size = 160. in
          let lengths =
            Map.map tag_map ~f:(fun l -> Float.of_int (List.length l))
          in
          let min, max =
            Map.fold lengths ~init:(Float.max_value, Float.min_value)
              ~f:(fun ~key:_ ~data (min, max) ->
                (Float.min min data, Float.max max data))
          in
          let normalized =
            Map.map lengths ~f:(fun len ->
                let r = (len -. min) /. (max -. min) in
                Float.to_int (min_size +. (r *. (max_size -. min_size))))
          in
          Map.to_alist normalized
          |> List.map ~f:(fun (tag, size) ->
                 Printf.sprintf {|<a style="font-size: %d%%;" href="%s">%s</a>|}
                   size (Tag.href tag) tag)
          |> String.concat
    in
    let body =
      render "templates/index.html"
        [ ("posts", render_post_items posts); ("tagcloud", tag_cloud) ]
    in
    render_default ~body ~title:"Home"
end

let run ~input ~output =
  let ops =
    match output with None -> Ops.dry_run | Some root -> Ops.real ~root
  in
  let posts = Post.load_all (input // "posts") in
  let tag_map =
    List.fold posts
      ~init:(Map.empty (module String))
      ~f:(fun m post ->
        Set.fold post.tags ~init:m ~f:(fun acc tag ->
            Map.add_multi acc ~key:tag ~data:post))
  in
  let all_tags = Map.keys tag_map in
  let rss_feed = rss_feed posts feed_config in
  Ops.mkdir ops ".";
  Ops.mkdir ops "posts";
  List.iter posts ~f:(Post.create ops);
  Ops.mkdir ops "tags";
  Ops.mkdir ops "feeds";
  List.iter all_tags ~f:(fun tag ->
      let posts = Map.find_multi tag_map tag in
      Tag.create_html ops feed_config tag posts;
      Tag.create_feed ops feed_config tag posts);
  Ops.create_file ops ~path:"rss.xml" ~contents:rss_feed;
  Ops.copy_files ops "static";
  let posts_contents = Templates.index posts ~tags:None in
  Ops.create_file ops ~path:"posts.html" ~contents:posts_contents;
  let index_contents =
    Templates.index (List.take posts 3) ~tags:(Some tag_map)
  in
  Ops.create_file ops ~path:"index.html" ~contents:index_contents

let info = Cmdliner.Cmd.info "blog"

let term =
  let open Cmdliner.Term.Syntax in
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
