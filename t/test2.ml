open Core.Std;;
open Core.In_channel;;

List.drop (read_lines "../test_data/master.20140214.idx") 7
|> List.map ~f:(fun x -> String.split ~on:'|' x)
|> List.sort ~cmp:(fun a b -> compare (List.nth_exn a 0) (List.nth_exn b 0))
|> List.group ~break:(fun a b -> (List.nth_exn a 0) <> (List.nth_exn b 0))
|> List.map ~f:(
  fun group ->
    List.foldi ~init:[] ~f:(
      fun row_ix accum row ->

        match row_ix > 0 with
        | false -> (List.nth_exn row 0) :: (List.nth_exn row 1) :: []
        | true ->
          if ( List.exists ~f:(fun cell -> cell = (List.nth_exn row 1)) accum ) then
            accum
          else
            accum @ (List.nth_exn row 1) :: []
    ) group
)
|> List.iter ~f:(
  fun row ->
    List.iter ~f:(fun cell -> Printf.printf "%s|" cell) row;
    Printf.printf "\n"
)
