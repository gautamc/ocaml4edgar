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

let () =
  let master_files = [
    ("../test_data/master.20140214-1-small.idx", 7); ("../test_data/master.20140214-2-small.idx", 7);
    ("../test_data/d2/master", 10); ("../test_data/d1/master", 10); ("../test_data/d1/master", 10);
  ] in
  let first_pair  = List.nth_exn master_files 0 and
      second_pair = List.nth_exn master_files 1
  in
  let rows_from_first_file  = sort_and_group_file ~skip_rows:(snd first_pair)  ~master_file:(fst first_pair)  ~field_index:0 ~field_to_pair:1 and
      rows_from_second_file = sort_and_group_file ~skip_rows:(snd second_pair) ~master_file:(fst second_pair) ~field_index:0 ~field_to_pair:1
  in
  printf "%d - %d\n" (List.length rows_from_first_file) (List.length rows_from_second_file);
