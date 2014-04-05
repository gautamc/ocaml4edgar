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

let visit_files_and_build_grouped_row () =
  (* ("../test_data/d1/master", 10) *)
  let master_files = [ ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7); ("../test_data/master.20140214-3-small.idx", 7); ] in
  let rec visit_file ~accum ~list_to_visit =
    match list_to_visit with
    | pair :: rest ->
      let rows = sort_and_group_file ~skip_rows:(snd pair) ~master_file:(fst pair) ~field_index:0 ~field_to_pair:1 in
      visit_file
        ~accum:(merge_via_collapsing_same_groups ~accum:accum ~in_list:rows)
        ~list_to_visit:rest
    | [] -> accum
  in
  List.iter ~f:(
    fun row ->
      List.iter ~f:(fun e -> printf "%s|" e) row;
      printf "\n";
  ) (visit_file ~accum:[] ~list_to_visit:master_files)
;;

let benchmark_merge_operation_across_files () =
  let master_files = [ ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7); ] in
  let first_pair = List.hd_exn master_files and
      second_pair = List.hd_exn (List.tl_exn master_files)
  in
  let rows_from_first_file = sort_and_group_file ~skip_rows:(snd first_pair) ~master_file:(fst first_pair) ~field_index:0 ~field_to_pair:1 and
      rows_from_second_file = sort_and_group_file ~skip_rows:(snd second_pair) ~master_file:(fst second_pair) ~field_index:0 ~field_to_pair:1
  in
  Command.run ~version: "0.1" ~build_info:"test program 3 with benchmarks" (
    Bench.make_command [
      Bench.Test.create ~name:"Merging rows from first file into empty list" (
        fun () ->
          ignore ( merge_via_collapsing_same_groups ~accum:[] ~in_list:rows_from_first_file )
      ) ;
      Bench.Test.create ~name:"Merging rows from second file into empty list" (
        fun () ->
          ignore ( merge_via_collapsing_same_groups ~accum:[] ~in_list:rows_from_second_file )
      ) ;
    ]
  );
;;

let command =
  Command.basic
    ~summary: "Test program for building and benchmarking group-by operation across multiple master files."
    ~readme: (fun () -> "")
    Command.Spec.(
      empty
      +> flag "-b" (no_arg) ~doc:"benchmark flag - If flag is set runs benchmark on merge_via_collapsing_same_groups else builds the grouped rows and prints them."
    )
    (
      fun benchmark () ->
        if( benchmark = true ) then
          benchmark_merge_operation_across_files ()
        else
          visit_files_and_build_grouped_row ()
    )
;;

let () =
  Command.run ~version: "0.1" ~build_info:"test program 3" command
;;
