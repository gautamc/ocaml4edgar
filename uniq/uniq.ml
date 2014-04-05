open Core.Std;;
open Sys;;
open Core.In_channel;;
open Unix;;
open Re2;;

let print_str_list list2print =
  match list2print with
  | [] -> printf "[]\n"
  | _ -> List.iter list2print ~f:(printf "%s\n")
;;

let listing_breakdown root_dir =
  fold_dir ~init:[[] ; []] ~f:(
    fun accum entry ->
      match (is_file (root_dir ^ "/" ^ entry)) with
      | `Yes -> [entry :: (List.nth_exn accum 0) ; List.nth_exn accum 1]
      | `No | `Unknown -> 
        match (is_directory (root_dir ^ "/" ^ entry)) with
        | `Yes -> [List.nth_exn accum 0 ; entry :: (List.nth_exn accum 1)]
        | `No | `Unknown -> accum
  ) root_dir
;;

let extract_uniq_field_values ~skip_rows ~master_file ~accum ~field_index =
  let records = List.map (List.drop (read_lines master_file) skip_rows) ~f:(fun x -> String.split ~on:'|' x) in
  let uniq_field_values = List.dedup (List.map records ~f:(fun xs -> List.nth_exn xs field_index)) in
  List.dedup (List.unordered_append accum uniq_field_values)
;;

let extract_uniq_field_pair_values ~skip_rows ~master_file ~accum ~field_index ~field_to_pair =
  let records = List.map (List.drop (read_lines master_file) skip_rows) ~f:(fun x -> String.split ~on:'|' x) in
  let uniq_field_pair_values = List.dedup
    (List.map records ~f:(fun xs -> (List.nth_exn xs field_index, List.nth_exn xs field_to_pair)))
    ~compare:(
      fun a b -> compare (fst a) (fst b)
    )
  in
  List.dedup (List.unordered_append accum uniq_field_pair_values) ~compare:(fun a b -> compare (fst a) (fst b))
;;

let merge_via_collapsing_same_groups ~accum in_list =
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

let extract_uniq_field_grouped_values ~skip_rows ~master_file ~accum ~field_index ~field_to_pair =
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
  |> merge_via_collapsing_same_groups ~accum:accum
;;

let rec preorder_listing ~values_list ~field_index root_dir =
  match listing_breakdown root_dir with
  | files :: dirs :: _ ->
    let uniq_values_list = List.fold files ~init:values_list ~f:(
      fun accum file ->
        if( Re2.Regex.matches (Re2.Regex.create_exn "^master\\.\\d+\\.idx$") file = true ) then
          extract_uniq_field_values ~skip_rows:7 ~master_file:(root_dir ^ "/" ^ file) ~accum:accum ~field_index:field_index
        else if ( Re2.Regex.matches (Re2.Regex.create_exn "^master$") file = true ) then
          extract_uniq_field_values ~skip_rows:10 ~master_file:(root_dir ^ "/" ^ file) ~accum:accum ~field_index:field_index
        else
          accum
    ) in
    List.fold dirs ~init:uniq_values_list ~f:(
      fun accum dir ->
        if( dir <> "data") then
          List.dedup (List.unordered_append accum (
            preorder_listing (root_dir ^ "/" ^ dir) ~values_list:accum ~field_index:field_index
          ))
        else
          accum
    )
  | _ :: [] | [] -> assert false
;;

let rec paired_preorder_listing ~values_list ~field_index ~field_to_pair root_dir =
  match listing_breakdown root_dir with
  | files :: dirs :: _ ->
    let uniq_values_list = List.fold files ~init:values_list ~f:(
      fun accum file ->
        if( Re2.Regex.matches (Re2.Regex.create_exn "^master\\.\\d+\\.idx$") file = true ) then
          extract_uniq_field_pair_values ~skip_rows:7 ~master_file:(root_dir ^ "/" ^ file) ~accum:accum ~field_index:field_index ~field_to_pair:field_to_pair
        else if ( Re2.Regex.matches (Re2.Regex.create_exn "^master$") file = true ) then
          extract_uniq_field_pair_values ~skip_rows:10 ~master_file:(root_dir ^ "/" ^ file) ~accum:accum ~field_index:field_index ~field_to_pair:field_to_pair
        else
          accum
    ) in
    List.fold dirs ~init:uniq_values_list ~f:(
      fun accum dir ->
        if( dir <> "data") then
          List.dedup (List.unordered_append accum (
            paired_preorder_listing (root_dir ^ "/" ^ dir) ~values_list:accum ~field_index:field_index ~field_to_pair:field_to_pair
          )) ~compare:(fun a b -> compare (fst a) (fst b))
        else
          accum
    )
  | _ :: [] | [] -> assert false
;;

let rec paired_grouped_preorder_listing ~values_list ~field_index ~field_to_pair root_dir =
  match listing_breakdown root_dir with
  | files :: dirs :: _ ->
    let uniq_values_list = List.fold files ~init:values_list ~f:(
      fun accum file ->
        if( Re2.Regex.matches (Re2.Regex.create_exn "^master\\.\\d+\\.idx$") file = true ) then
          extract_uniq_field_grouped_values ~skip_rows:7 ~master_file:(root_dir ^ "/" ^ file) ~accum:accum ~field_index:field_index ~field_to_pair:field_to_pair
        else if ( Re2.Regex.matches (Re2.Regex.create_exn "^master$") file = true ) then
          extract_uniq_field_grouped_values ~skip_rows:10 ~master_file:(root_dir ^ "/" ^ file) ~accum:accum ~field_index:field_index ~field_to_pair:field_to_pair
        else
          accum
    ) in
    List.fold dirs ~init:uniq_values_list ~f:(
      fun accum dir ->
        if( dir <> "data") then
          paired_grouped_preorder_listing (root_dir ^ "/" ^ dir) ~values_list:accum ~field_index:field_index ~field_to_pair:field_to_pair
        else
          accum
    )
  | _ :: [] | [] -> assert false
;;

let typed_preorder_listing ~field_index ~field_to_pair ~group_p root_dir =
  match group_p with
  | true -> (
    match field_to_pair with
    | Some field_to_pair ->
      let uniq_field_values = paired_grouped_preorder_listing root_dir ~values_list:[] ~field_index:field_index ~field_to_pair:field_to_pair in
      List.iter uniq_field_values ~f:(
        fun row ->
          List.fold ~init:"" ~f:(
            fun line_accum cell ->
              if( line_accum <> "" ) then
                line_accum ^ "|" ^ cell
              else
                line_accum ^ cell
          ) row |> printf "%s\n";
      )
    | None -> assert false
  )
  | false -> (
    match field_to_pair with
    | Some field_to_pair ->
      let uniq_field_values = paired_preorder_listing root_dir ~values_list:[] ~field_index:field_index ~field_to_pair:field_to_pair in
      List.iter uniq_field_values ~f:(fun pair -> printf "%s,\"%s\"\n" (fst pair) (snd pair))
    | None ->
      let uniq_field_values = preorder_listing root_dir ~values_list:[] ~field_index:field_index in
      print_str_list uniq_field_values
  )
;;

let command =
  Command.basic
    ~summary: "Build a list of unique values for the given field by recursively visiting index files under the root directory's daily-index and full-index sub-directories."
    ~readme: (
      fun () ->
        ""
    )
    Command.Spec.(
      empty
      +> flag "-f" (required int) ~doc:"field_index Zero based index of field to extract unique values from."
      +> flag "-p" (optional int) ~doc:"pair of fields to extract unique values from. "
      +> flag "-g" (no_arg) ~doc:"group non-unique values"
      +> anon ("root_dir" %: file)
    )
    (
      fun field_index field_to_pair group_p root_dir () ->
        typed_preorder_listing root_dir ~field_index:field_index ~field_to_pair:field_to_pair ~group_p:group_p
    )
;;

let () =
  Command.run ~version: "0.1" ~build_info: "NOGIT" command
