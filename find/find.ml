open Core.Std;;
open Printf;;
open Core.In_channel;;

let rec process_lines ?ix ~is_quaterly master_file_handle =
  let ix = match ix with
    | None -> 0
    | Some x -> x
  and skip_count = match is_quaterly with
    | true -> 9
    | false -> 6
  in
  match input_line master_file_handle with
  | Some line -> (
    if ix > skip_count then (
      let file_rel_path = List.nth_exn (String.split line ~on:'|') 4 in
      match Sys.file_exists ("/data/yacct/" ^ file_rel_path) with
      | `Yes -> (* printf "Found: %d ==> %s\n" ix line *) ()
      | `No -> printf "%s\n" line
      | `Unknown -> printf "%s\n" line
    );
    process_lines ~ix:(ix+1) ~is_quaterly:is_quaterly master_file_handle
  )
  | None -> close master_file_handle
;;

let command =
  Command.basic
    ~summary: "Find edgar files that havn't been downloaded."
    ~readme: (
      fun () ->
        "yacct.com backend command - takes as input a master file (daily or quaterly) and prints as output records that have not been downloaded."
    )
    Command.Spec.(
      empty
      +> flag "-q" no_arg ~doc:" file given is a quaterly master file. If this switch is not given then assumes a daily master file"
      +> anon ("master_file" %: file)
    )
    (fun is_quaterly filename () -> process_lines ~is_quaterly:is_quaterly (create filename))
;;

let () =
  Command.run ~version: "0.1" ~build_info: "NOGIT" command
