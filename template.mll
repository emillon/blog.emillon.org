{
  open Base
  open Stdio
}
let id = ['a'-'z']+

rule template env ofmt = parse
| [^ '$']+
{ Stdlib.Format.fprintf ofmt "%s" (Lexing.lexeme lexbuf);
  template env ofmt lexbuf
}
| "$if(" id ")$"
{ template env ofmt lexbuf
}
| "$endif$"
{ template env ofmt lexbuf
}
| '$' (id as id) '$'
{
  match Map.find env id with
  | None -> Printf.ksprintf failwith "Unknown variable: %s" id
  | Some v ->
    begin
      Stdlib.Format.fprintf ofmt "%s" v;
      template env ofmt lexbuf
    end
}
| eof { () }

{
  let process env ~input ~output =
    In_channel.with_file input ~f:(fun ic ->
      let lexbuf = Lexing.from_channel ic in
      template env output lexbuf;
      Stdlib.Format.pp_print_flush output ()
    )
}
