open Core.Std;;
open Unix;;
open Core.In_channel;;
open Grouper;;
open Core_bench.Std;;

let () =
  let master_files = [
    ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7); ("../test_data/master.20140214-3-small.idx", 7);
    ("../test_data/master.20140314.idx", 7); ("../test_data/d1/master", 10); ("../test_data/d2/master", 10);
  ] in
  let first_pair  = List.nth_exn master_files 0 and
      second_pair = List.nth_exn master_files 1 and
      third_pair  = List.nth_exn master_files 2
  in
  let start_time_op1 = Unix.gettimeofday() in
  let rows_from_first_file  = sort_and_group_file ~skip_rows:(snd first_pair)  ~master_file:(fst first_pair)  ~field_index:0 ~field_to_pair:1 and
      rows_from_second_file = sort_and_group_file ~skip_rows:(snd second_pair) ~master_file:(fst second_pair) ~field_index:0 ~field_to_pair:1 and
      rows_from_third_file  = sort_and_group_file ~skip_rows:(snd third_pair) ~master_file:(fst third_pair) ~field_index:0 ~field_to_pair:1 in
  let start_time_op2 = Unix.gettimeofday() in
  let grouped_rows = merge_via_collapsing_same_groups ~accum:rows_from_first_file ~in_list:rows_from_second_file in
  let start_time_op3 = Unix.gettimeofday() in
  let grouped_rows_2 = merge_via_collapsing_same_groups ~accum:grouped_rows ~in_list:rows_from_third_file in
  let end_time = Unix.gettimeofday() in
  printf "%f ; %f : %f\n" start_time_op1 start_time_op2 (start_time_op2 -. start_time_op1);
  printf "%f ; %f : %f\n" start_time_op2 start_time_op3 (start_time_op3 -. start_time_op2);
  printf "%f ; %f : %f\n" start_time_op3 end_time (end_time -. start_time_op3);

  printf "%d\n" (List.length grouped_rows);
  printf "%d\n" (List.length grouped_rows_2);
  printf "%d - %d - %d\n" (List.length rows_from_first_file) (List.length rows_from_second_file) (List.length rows_from_third_file);

  List.iter ~f:(
    fun row ->
      List.reduce_exn ~f:(fun a b -> a ^ "|" ^ b) row |> printf "%s\n"
  ) rows_from_first_file;

  List.iter ~f:(
    fun row ->
      List.reduce_exn ~f:(fun a b -> a ^ "|" ^ b) row |> printf "%s\n"
  ) rows_from_second_file;

  List.iter ~f:(
    fun row ->
      List.reduce_exn ~f:(fun a b -> a ^ "|" ^ b) row |> printf "%s\n"
  ) rows_from_third_file;

  List.iter ~f:(
    fun row ->
      List.reduce_exn ~f:(fun a b -> a ^ "|" ^ b) row |> printf "%s\n"
  ) grouped_rows;
 
  List.iter ~f:(
    fun row ->
      List.reduce_exn ~f:(fun a b -> a ^ "|" ^ b) row |> printf "%s\n"
  ) grouped_rows_2;
