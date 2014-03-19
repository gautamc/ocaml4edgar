open Core.Std;;
open Sys;;

(*

[ ["11112" ; "TEST1" ; "FORM1"] ; ["11112" ; "TEST1" ; "FORM2"] ; ["11113" ; "TEST2" ; "FORM1"] ; ["11113" ; "TEST2" ; "FORM2"] ; ["11114" ; "TEST3" ; "FORM1"] ]
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
(* List.group ~break:(fun a b -> (List.nth_exn a 0) <> (List.nth_exn b 0)) *)

*)

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

let rec preorder_listing root_dir =
  match listing_breakdown root_dir with
  | files :: dirs :: _ ->
    List.iter ~f:(fun e -> Printf.printf "%s/%s\n" root_dir e) files;
    List.iter ~f:(fun e -> if( e <> "data") then preorder_listing (root_dir ^ "/" ^ e)) dirs
  | _ :: [] | [] -> assert false
;;


preorder_listing Sys.argv.(1)
