open Core.Std;;
open Unix;;
open Core.In_channel;;
open Grouper;;
open Core_bench.Std;;

let () =
  let master_files = [
    ("../test_data/master.20140401-21600.idx",7); ("../test_data/master.20140402-21600.idx", 7); ("../test_data/master.20140403-21600.idx", 7);
    ("../test_data/master.20140401-14400.idx",7); ("../test_data/master.20140402-14400.idx", 7); ("../test_data/master.20140403-14400.idx", 7);
    ("../test_data/master.20140401-7200.idx",7); ("../test_data/master.20140402-7200.idx", 7); ("../test_data/master.20140403-7200.idx", 7);
    ("../test_data/master.20140401-2400.idx",7); ("../test_data/master.20140402-2400.idx", 7); ("../test_data/master.20140403-2400.idx", 7);
    ("../test_data/master.20140401-800.idx",7); ("../test_data/master.20140402-800.idx", 7); ("../test_data/master.20140403-800.idx", 7);
    ("../test_data/master.20140401-400.idx",7); ("../test_data/master.20140402-400.idx", 7); ("../test_data/master.20140403-400.idx", 7);
    ("../test_data/master.20140401-200.idx",7); ("../test_data/master.20140402-200.idx", 7); ("../test_data/master.20140403-200.idx", 7);
    ("../test_data/master.20140401-100.idx",7); ("../test_data/master.20140402-100.idx", 7); ("../test_data/master.20140403-100.idx", 7);
    ("../test_data/master.20140214-1-small-2x.idx",7); ("../test_data/master.20140214-2-small-2x.idx", 7); ("../test_data/master.20140214-3-small-2x.idx", 7);
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
  printf "ALL Sort OPS => Start Time: %f, End Time: %f, Total Time: %f\n" start_time_op1 start_time_op2 (start_time_op2 -. start_time_op1);
  printf "Group OP 1 => Start Time: %f, End Time: %f, Total Time: %f\n" start_time_op2 start_time_op3 (start_time_op3 -. start_time_op2);
  printf "Group OP 2 => Start Time %f, End Time: %f, Total Time: %f\n" start_time_op3 end_time (end_time -. start_time_op3);

(*
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
*)


(****************

OUTPUT WITH -small.idx files: 

ALL Sort OPS => Start Time: 1397341986.189666, End Time: 1397341986.189731, Total Time: 0.000065
Group OP 1 => Start Time: 1397341986.189731, End Time: 1397341986.189734, Total Time: 0.000003
Group OP 2 => Start Time 1397341986.189734, End Time: 1397341986.189736, Total Time: 0.000002

OUTPUT WITH -small-2x.idx files: 

ALL Sort OPS => Start Time: 1397343415.049344, End Time: 1397343415.049418, Total Time: 0.000074
Group OP 1 => Start Time: 1397343415.049418, End Time: 1397343415.049422, Total Time: 0.000004
Group OP 2 => Start Time 1397343415.049422, End Time: 1397343415.049424, Total Time: 0.000002

head -107 /data/yacct/edgar/daily-index/2014_04/master.20140401.idx > ../test_data/master.20140401-100.idx
head -107 /data/yacct/edgar/daily-index/2014_04/master.20140402.idx > ../test_data/master.20140402-100.idx
head -107 /data/yacct/edgar/daily-index/2014_04/master.20140403.idx > ../test_data/master.20140403-100.idx
OUTPUT WITH -100.idx files: 

ALL Sort OPS => Start Time: 1397346280.899281, End Time: 1397346280.899676, Total Time: 0.000395
Group OP 1 => Start Time: 1397346280.899676, End Time: 1397346280.899777, Total Time: 0.000101
Group OP 2 => Start Time 1397346280.899777, End Time: 1397346280.900062, Total Time: 0.000285

head -207 /data/yacct/edgar/daily-index/2014_04/master.20140401.idx > ../test_data/master.20140401-200.idx
head -207 /data/yacct/edgar/daily-index/2014_04/master.20140402.idx > ../test_data/master.20140402-200.idx
head -207 /data/yacct/edgar/daily-index/2014_04/master.20140403.idx > ../test_data/master.20140403-200.idx
OUTPUT WITH -200.idx files: 

ALL Sort OPS => Start Time: 1397345989.763435, End Time: 1397345989.764147, Total Time: 0.000712
Group OP 1 => Start Time: 1397345989.764147, End Time: 1397345989.764824, Total Time: 0.000677
Group OP 2 => Start Time 1397345989.764824, End Time: 1397345989.766911, Total Time: 0.002087

head -407 /data/yacct/edgar/daily-index/2014_04/master.20140401.idx > ../test_data/master.20140401-400.idx
head -407 /data/yacct/edgar/daily-index/2014_04/master.20140402.idx > ../test_data/master.20140402-400.idx
head -407 /data/yacct/edgar/daily-index/2014_04/master.20140403.idx > ../test_data/master.20140403-400.idx
OUTPUT WITH -400.idx files: 

ALL Sort OPS => Start Time: 1397346664.095869, End Time: 1397346664.097269, Total Time: 0.001400
Group OP 1 => Start Time: 1397346664.097269, End Time: 1397346664.100708, Total Time: 0.003439
Group OP 2 => Start Time 1397346664.100708, End Time: 1397346664.105576, Total Time: 0.004868

head -807 /data/yacct/edgar/daily-index/2014_04/master.20140401.idx > ../test_data/master.20140401-800.idx
head -807 /data/yacct/edgar/daily-index/2014_04/master.20140402.idx > ../test_data/master.20140402-800.idx
head -807 /data/yacct/edgar/daily-index/2014_04/master.20140403.idx > ../test_data/master.20140403-800.idx
OUTPUT WITH -800.idx files: 

ALL Sort OPS => Start Time: 1397347856.018215, End Time: 1397347856.020838, Total Time: 0.002623
Group OP 1 => Start Time: 1397347856.020838, End Time: 1397347856.034244, Total Time: 0.013406
Group OP 2 => Start Time 1397347856.034244, End Time: 1397347856.054529, Total Time: 0.020285


head -2407 /data/yacct/edgar/daily-index/2014_04/master.20140401.idx > ../test_data/master.20140401-2400.idx
head -2407 /data/yacct/edgar/daily-index/2014_04/master.20140402.idx > ../test_data/master.20140402-2400.idx
head -2407 /data/yacct/edgar/daily-index/2014_04/master.20140403.idx > ../test_data/master.20140403-2400.idx
OUTPUT WITH -2400.idx files:

ALL Sort OPS => Start Time: 1397348172.325642, End Time: 1397348172.335305, Total Time: 0.009663
Group OP 1 => Start Time: 1397348172.335305, End Time: 1397348172.533438, Total Time: 0.198133
Group OP 2 => Start Time 1397348172.533438, End Time: 1397348172.851040, Total Time: 0.317602

head -7207 ../test_data/d1/master > ../test_data/master.20140401-7200.idx
head -7207 ../test_data/d2/master > ../test_data/master.20140402-7200.idx
head -7207 /data/yacct/edgar/full-index/2013/QTR1/master > ../test_data/master.20140403-7200.idx
OUTPUT WITH -7200.idx files:

ALL Sort OPS => Start Time: 1397348668.672051, End Time: 1397348668.701508, Total Time: 0.029457
Group OP 1 => Start Time: 1397348668.701508, End Time: 1397348669.920445, Total Time: 1.218937
Group OP 2 => Start Time 1397348669.920445, End Time: 1397348671.887200, Total Time: 1.966755

head -14407 ../test_data/d1/master > ../test_data/master.20140401-14400.idx
head -14407 ../test_data/d2/master > ../test_data/master.20140402-14400.idx
head -14407 /data/yacct/edgar/full-index/2013/QTR1/master > ../test_data/master.20140403-14400.idx
OUTPUT WITH -14400.idx files:

ALL Sort OPS => Start Time: 1397357933.689751, End Time: 1397357933.722172, Total Time: 0.032421
Group OP 1 => Start Time: 1397357933.722172, End Time: 1397357935.040020, Total Time: 1.317848
Group OP 2 => Start Time 1397357935.040020, End Time: 1397357937.149802, Total Time: 2.109782

head -21607 ../test_data/d1/master > ../test_data/master.20140401-21600.idx
head -21607 ../test_data/d2/master > ../test_data/master.20140402-21600.idx
head -21607 /data/yacct/edgar/full-index/2013/QTR1/master > ../test_data/master.20140403-21600.idx
OUTPUT WITH -21600.idx files:

ALL Sort OPS => Start Time: 1397360396.122938, End Time: 1397360396.154486, Total Time: 0.031548
Group OP 1 => Start Time: 1397360396.154486, End Time: 1397360397.456831, Total Time: 1.302345
Group OP 2 => Start Time 1397360397.456831, End Time: 1397360399.672941, Total Time: 2.216110

****************)
