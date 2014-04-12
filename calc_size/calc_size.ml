open Sys;;
open Core.Std;;
open Core.In_channel;;
open Printf;;
open Unix;;
open Int64;;

let ls dir_path_str =
  match dir_path_str with
  | "" -> assert false
  | _ -> (
    let dir_handle = opendir dir_path_str in
    let rec dir_reader accum =
      match
        try Some (readdir dir_handle) with
        | End_of_file -> None
      with
      | Some x -> dir_reader (x :: accum)
      | None -> accum
    in
      dir_reader []
  )
;;

let rec _calc_disk_space_helper file_paths size_accum =
  match file_paths with
  | [] -> size_accum
  | a_file :: rest -> (
    match
      try Some (stat ("/data/yacct/" ^ a_file)) with
      | _ -> None
    with
    | Some file_stat_rec -> _calc_disk_space_helper rest size_accum+file_stat_rec.st_size
    | None -> _calc_disk_space_helper rest size_accum
  )
;;

let calc_disk_space file_paths =
  match file_paths with
  | [] -> 0L
  | _ -> _calc_disk_space_helper file_paths 0L
;;


let space_used master_file =
  match master_file with
  | "" -> assert false
  | _ -> (
    let records = List.map (List.drop (read_lines master_file) 7) ~f:(fun x -> String.split ~on:'|' x) in
    let disk_space_used = calc_disk_space (List.dedup (List.map records ~f:(fun xs -> List.nth_exn xs 4))) in
    (to_float disk_space_used) /. (1024.0 *. 1024.0 *. 1024.0)
  )
;;

if of_int (Array.length Sys.argv) >= 2L then begin
  ls Sys.argv.(1)
  |> List.filter ~f:(fun x -> Re2.Regex.matches (Re2.Regex.create_exn "^.+\\.idx$") x)
  |> List.map ~f:(
    fun master_file_name -> space_used (Sys.argv.(1) ^ master_file_name)
  ) |> List.fold ~init:0.0 ~f:(fun total size -> size +. total ) |> printf "%f GB\n"
end
