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

let file_sizes master_file =
  []
;;

if of_int (Array.length Sys.argv) >= 2L then begin
  ls Sys.argv.(1)
  |> List.filter ~f:(fun x -> Re2.Regex.matches (Re2.Regex.create_exn "^.+\\.idx$") x)
  |> List.map ~f:(
    fun master_file_name -> file_sizes (Sys.argv.(1) ^ master_file_name)
  ) |> List.length |> printf "%d\n"
end
