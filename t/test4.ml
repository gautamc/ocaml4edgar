open Core.Std;;
open Core.In_channel;;
open Core_bench.Std;;

let sort_and_group_file ~skip_rows ~master_file ~field_index ~field_to_pair =
  List.drop (read_lines master_file) skip_rows
  |> List.map ~f:(fun x -> String.split ~on:'|' x)
  |> List.sort ~cmp:(fun a b -> compare (List.nth_exn a field_index) (List.nth_exn b field_index))
  |> List.group ~break:(fun a b -> List.nth_exn a field_index <> List.nth_exn b field_index)
  |> List.map ~f:(
    fun group ->
      List.foldi ~init:[] ~f:(
        fun row_ix group_accum row ->
          match row_ix > 0 with
          | false -> (List.nth_exn row field_index) :: (List.nth_exn row field_to_pair) :: []
          | true ->
            if ( List.exists ~f:(fun cell -> cell = (List.nth_exn row field_to_pair)) group_accum ) then
              group_accum
            else
              group_accum @ (List.nth_exn row field_to_pair) :: []
      ) group
  )
;;

let merge_via_collapsing_same_groups ~accum ~in_list =
  List.fold ~init:accum ~f:(
    fun accum grouped_row ->
      match List.partition_tf ~f:(
        fun accum_row ->
          (List.nth_exn accum_row 0) = (List.nth_exn grouped_row 0)
      ) accum
      with
      | ([], _) -> grouped_row :: accum
      | (row :: [], filtered_accum) ->
        ((List.hd_exn row) :: ((List.tl_exn row) @ (List.tl_exn grouped_row) |> List.dedup)) :: filtered_accum  
      | (_,_) -> assert false
  ) in_list
;;

let () =
  let master_files = [ ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7); ("../test_data/d2/master", 10); ("../test_data/d1/master", 10); ] in
  let first_pair = List.hd_exn master_files and
      second_pair = List.hd_exn (List.tl_exn master_files)
  in
  let rows_from_first_file = sort_and_group_file ~skip_rows:(snd first_pair) ~master_file:(fst first_pair) ~field_index:0 ~field_to_pair:1 and
      rows_from_second_file = sort_and_group_file ~skip_rows:(snd second_pair) ~master_file:(fst second_pair) ~field_index:0 ~field_to_pair:1
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
      )
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

OUTPUT WITH THE TWO ACTUAL MASTER FILES (../test_data/d2/master", ../test_data/d1/master)
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
*)
