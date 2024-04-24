{
  open Base
  open Stdio

  let active_output env ids =
    List.for_all ids ~f:(Map.mem env)
}
let id = ['a'-'z']+

rule template env stack ofmt = parse
| [^ '$']+
{
    if active_output env stack then
      Stdlib.Format.fprintf ofmt "%s" (Lexing.lexeme lexbuf);
    template env stack ofmt lexbuf
}
| "$if(" (id as id) ")$"
{
    template env (id::stack) ofmt lexbuf
}
| "$endif$"
{
    template env (List.tl_exn stack) ofmt lexbuf
}
| '$' (id as id) '$'
{
    if active_output env stack then
      match Map.find env id with
      | None -> Printf.ksprintf failwith "Unknown variable: %s" id
      | Some v ->
        begin
          Stdlib.Format.fprintf ofmt "%s" v;
          template env stack ofmt lexbuf
        end
}
| eof { () }

{
  let process env ~input ~output =
    In_channel.with_file input ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      let stack = [] in
      template env stack output lexbuf;
      Stdlib.Format.pp_print_flush output ()
    )
}
