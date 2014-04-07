open Sys;;
open Core.Std;;
open Core.In_channel;;
open Printf;;
open Unix;;

let all_lines = read_lines "/data/yacct/edgar/daily-index/2014_01/master.20140110.idx" in
  printf "Total Lines: %d\n" (List.length all_lines);
  match
    try (List.nth all_lines 7) with
    | _ -> None
  with
    | None -> assert false
    | Some x ->
      printf "%s\n" x;
