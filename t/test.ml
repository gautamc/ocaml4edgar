open Core.Std;;
open Sys;;

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
