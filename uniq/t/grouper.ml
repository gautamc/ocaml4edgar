open Core.Std;;
open Core.In_channel;;

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

(**
For each row in in_list (referred to as this_in_list_row)
  Partition accum into two lists:
    "First list" has rows from accum which have a CIK that is also present in this_in_list_row
    "Second list" has rows from accum which have a CIK that is not present in this_in_list_row
      if the "First list" is empty then append this_in_list_row to accum (because this is the first instance of CIK in accum)
      if the "First list" has one element then
        append company names from this_in_list_row to the element in "First List" while ensuring there are no duplicate company names
        append this new list of CIK and company names to the "Second List"
      if the "First List" has more than one element then raise an error, because this should never happen.
        The previous case will always ensure that all company names which have the same CIK are appended onto the same list.
    
**)
let merge_via_collapsing_same_groups ~accum ~in_list =
  List.fold ~init:accum ~f:(
    fun accum this_in_list_row ->
      match List.partition_tf ~f:(
        fun accum_row ->
          (List.nth_exn accum_row 0) = (List.nth_exn this_in_list_row 0)
      ) accum
      with
      | ([], _) -> this_in_list_row :: accum
      | (row :: [], filtered_accum) ->
        ((List.hd_exn row) :: ((List.tl_exn row) @ (List.tl_exn this_in_list_row) |> List.dedup)) :: filtered_accum  
      | (_,_) -> assert false
  ) in_list
;;
