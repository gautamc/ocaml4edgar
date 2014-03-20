open Core.Std;;
open Core.In_channel;;
open Core_bench.Std;;

let collapse_groups groups =
  List.map ~f:(
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
  ) groups
;;

let read_sort_group_rows () =
  List.drop (read_lines "../test_data/master.20140214.idx") 7
  |> List.map ~f:(fun x -> String.split ~on:'|' x)
  |> List.sort ~cmp:(fun a b -> compare (List.nth_exn a 0) (List.nth_exn b 0))
  |> List.group ~break:(fun a b -> (List.nth_exn a 0) <> (List.nth_exn b 0))  
;;

let main () =
  read_sort_group_rows ()
  |> collapse_groups
  |> List.iter ~f:(
    fun row ->
      List.iter ~f:(fun cell -> Printf.printf "%s|" cell) row;
      Printf.printf "\n";
  )
;;

let () =
  let records = read_sort_group_rows () in
  Command.run (
    Bench.make_command [
      Bench.Test.create ~name:"Read Sort Group operation" (
        fun () ->
          ignore (read_sort_group_rows ())
      ) ;
      Bench.Test.create ~name:"Collapse Groups operation" (
        fun () ->
          ignore (collapse_groups records)
      )
    ]
  );

(************* OUTPUT *************

Estimated testing time 20s (2 benchmarks x 10s). Change using -quota SECS.
┌───────────────────────────┬──────────┬────────────┬──────────┬──────────┬────────────┐
│ Name                      │ Time/Run │    mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────────────┼──────────┼────────────┼──────────┼──────────┼────────────┤
│ Read Sort Group operation │  34.51ms │ 2_968.19kw │ 869.98kw │ 869.97kw │    100.00% │
│ Collapse Groups operation │   1.73ms │   315.75kw │  11.11kw │  11.11kw │      5.00% │
└───────────────────────────┴──────────┴────────────┴──────────┴──────────┴────────────┘

***********************************)
