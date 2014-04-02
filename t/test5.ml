open Core.Std;;
open Unix;;
open Core.In_channel;;
open Grouper;;
open Core_bench.Std;;

let () =
  let master_files = [
    ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7);
    ("../test_data/master.20140214-2-small.idx", 7);
    ("../test_data/d2/master", 10); ("../test_data/d1/master", 10); ("../test_data/d1/master", 10);
  ] in
  let first_pair  = List.nth_exn master_files 0 and
      second_pair = List.nth_exn master_files 1 and
      third_pair  = List.nth_exn master_files 2
  in
  let start_time = Unix.gettimeofday() in
  let rows_from_first_file  = sort_and_group_file ~skip_rows:(snd first_pair)  ~master_file:(fst first_pair)  ~field_index:0 ~field_to_pair:1 and
      rows_from_second_file = sort_and_group_file ~skip_rows:(snd second_pair) ~master_file:(fst second_pair) ~field_index:0 ~field_to_pair:1 and
      rows_from_third_file  = sort_and_group_file ~skip_rows:(snd second_pair) ~master_file:(fst third_pair) ~field_index:0 ~field_to_pair:1 in
  let end_time = Unix.gettimeofday() in
  printf "%f ; %f : %f\n" start_time end_time (end_time -. start_time);
  printf "%d - %d - %d\n" (List.length rows_from_first_file) (List.length rows_from_second_file) (List.length rows_from_third_file);
