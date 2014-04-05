open Core.Std;;
open Core.In_channel;;
open Grouper;;
open Core_bench.Std;;

let () =
  let master_files = [
    ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7);
    ("../test_data/d2/master", 10); ("../test_data/d1/master", 10); ("../test_data/d1/master", 10);
  ] in
  let first_pair  = List.nth_exn master_files 0 and
      second_pair = List.nth_exn master_files 1 and
      third_pair  = List.nth_exn master_files 2
  in
  let rows_from_first_file  = sort_and_group_file ~skip_rows:(snd first_pair)  ~master_file:(fst first_pair)  ~field_index:0 ~field_to_pair:1 and
      rows_from_second_file = sort_and_group_file ~skip_rows:(snd second_pair) ~master_file:(fst second_pair) ~field_index:0 ~field_to_pair:1 and
      rows_from_third_file  = sort_and_group_file ~skip_rows:(snd third_pair)  ~master_file:(fst third_pair)  ~field_index:0 ~field_to_pair:1
  in
  let
      merged_rows_from_first_and_second_files = merge_via_collapsing_same_groups ~accum:rows_from_first_file ~in_list:rows_from_second_file
  in
  Command.run (
    Bench.make_command [
      Bench.Test.create ~name:"Merging rows from first file into empty list" (
        fun () ->
          ignore ( merge_via_collapsing_same_groups ~accum:[] ~in_list:rows_from_first_file )
      ) ;
      Bench.Test.create ~name:"Merging rows from second file into empty list" (
        fun () ->
          ignore ( merge_via_collapsing_same_groups ~accum:[] ~in_list:rows_from_second_file )
      ) ;
      Bench.Test.create ~name:"Merging rows from second file into rows from first file" (
        fun () ->
          ignore ( merge_via_collapsing_same_groups ~accum:rows_from_first_file ~in_list:rows_from_second_file )
      ) ;
      Bench.Test.create ~name:"Merging rows from third file into merged_rows_from_first_and_second_files" (
        fun () ->
          ignore ( merge_via_collapsing_same_groups ~accum:merged_rows_from_first_and_second_files ~in_list:rows_from_third_file )
      ) ;
    ]
  );
;;

(*

OUTPUT WITH THE TWO SMALL FILES:
$ ./test4/test4.native 
Estimated testing time 30s (3 benchmarks x 10s). Change using -quota SECS.
┌─────────────────────────────────────────────────────────┬──────────┬─────────┬────────────┐
│ Name                                                    │ Time/Run │ mWd/Run │ Percentage │
├─────────────────────────────────────────────────────────┼──────────┼─────────┼────────────┤
│ Merging rows from first file into empty list            │  76.85ns │  48.00w │      9.28% │
│ Merging rows from second file into empty list           │  76.84ns │  48.00w │      9.27% │
│ Merging rows from second file into rows from first file │ 828.59ns │ 233.00w │    100.00% │
└─────────────────────────────────────────────────────────┴──────────┴─────────┴────────────┘

OUTPUT WITH THE TWO ACTUAL MASTER FILES (../test_data/d2/master, ../test_data/d1/master)
$ ./test4/test4.native 
Estimated testing time 30s (3 benchmarks x 10s). Change using -quota SECS.
┌─────────────────────────────────────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                                                    │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├─────────────────────────────────────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ Merging rows from first file into empty list            │  127.16s │ 20.30Gw │   1.39Gw │   1.39Gw │     27.26% │
│ Merging rows from second file into empty list           │  148.02s │ 21.59Gw │   1.53Gw │   1.53Gw │     31.74% │
│ Merging rows from second file into rows from first file │  466.43s │ 50.36Gw │  11.83Gw │  11.83Gw │    100.00% │
└─────────────────────────────────────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
Benchmarks that take 1ns to 100ms can be estimated precisely. For more reliable 
estimates, redesign your benchmark to have a shorter execution time.

Estimated testing time 40s (4 benchmarks x 10s). Change using -quota SECS.
┌───────────────────────────────────────────────────────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                                                                      │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────────────────────────────────────────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ Merging rows from first file into empty list                              │  140.21s │ 20.30Gw │   1.39Gw │   1.39Gw │     17.19% │
│ Merging rows from second file into empty list                             │  157.23s │ 21.59Gw │   1.53Gw │   1.53Gw │     19.28% │
│ Merging rows from second file into rows from first file                   │  547.16s │ 50.36Gw │  11.83Gw │  11.83Gw │     67.08% │
│ Merging rows from third file into merged_rows_from_first_and_second_files │  815.68s │ 60.56Gw │  21.30Gw │  21.30Gw │    100.00% │
└───────────────────────────────────────────────────────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
Benchmarks that take 1ns to 100ms can be estimated precisely. For more reliable 
estimates, redesign your benchmark to have a shorter execution time.

*)
