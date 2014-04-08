open Sys;;
open Core.Std;;
open Core.In_channel;;
open Printf;;
open Unix;;

let ls dir_path_str =
  match dir_path_str with
  | "" -> assert false
  | _ -> (
    let dir_handle = opendir dir_path_str in
    let rec dir_reader accum =
      match
        try Some (readdir dir_handle) with
        | End_of_file -> None
      with
      | Some x -> dir_reader (x :: accum)
      | None -> accum
    in
      dir_reader []
  )
;;

ignore (ls Sys.argv.(1))

(*
let all_lines = read_lines "/data/yacct/edgar/daily-index/2014_01/master.20140110.idx" in
  printf "Total Lines: %d\n" (List.length all_lines);
  match
    try (List.nth all_lines 7) with
    | _ -> None
  with
    | None -> assert false
    | Some x ->
      printf "%s\n" x;
      *)
