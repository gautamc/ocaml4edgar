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
      match List.partition_tf ~f:(fun e -> false) accum with
      | (rows, filtered_accum) -> grouped_row :: filtered_accum
      | ([], filtered_accum) -> grouped_row :: filtered_accum
      | ([], []) -> assert false
  ) in_list
;;

let () =
  let master_files = [ ("../test_data/master.20140214.idx", 7); ("../test_data/d1/master", 10); ] in
  let rec visit_file ~accum ~list_to_visit =
    match list_to_visit with
    | pair :: rest ->
      let rows = sort_and_group_file ~skip_rows:(snd pair) ~master_file:(fst pair) ~field_index:0 ~field_to_pair:1 in
      visit_file
        ~accum:(merge_via_collapsing_same_groups ~accum:accum ~in_list:rows)
        ~list_to_visit:rest
    | [] -> accum
  in
  printf "%d\n" (List.length (visit_file ~accum:[] ~list_to_visit:master_files))
;;
