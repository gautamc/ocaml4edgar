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
        if( List.exists ~f:(fun cell_val -> cell_val = (List.nth_exn grouped_row 1)) row ) then
          accum
        else
          (row @ [List.nth_exn grouped_row 1]) :: filtered_accum  
      | (_,_) -> assert false
  ) in_list
;;

let () =
  (* ("../test_data/d1/master", 10); *)
  let master_files = [ ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7); ("../test_data/master.20140214-3-small.idx", 7) ] in
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
