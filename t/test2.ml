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
  let raw_records = List.drop (read_lines "../test_data/master.20140214.idx") 7 and records = read_sort_group_rows () in
  let records_to_sort = List.map ~f:(fun x -> String.split ~on:'|' x) raw_records in
  let records_to_group = List.sort ~cmp:(fun a b -> compare (List.nth_exn a 0) (List.nth_exn b 0)) records_to_sort in
  Command.run (
    Bench.make_command [
      Bench.Test.create ~name:"Read Sort Group operation" (
        fun () ->
          ignore (read_sort_group_rows ())
      ) ;
      Bench.Test.create ~name:"Collapse Groups operation" (
        fun () ->
          ignore (collapse_groups records)
      ) ;
      Bench.Test.create ~name:"read_lines operation" (
        fun () ->
          ignore (
            List.drop (read_lines "../test_data/master.20140214.idx") 7
          )
      ) ;
      Bench.Test.create ~name:"String to List map operation" (
        fun () ->
          ignore (
            List.map ~f:(fun x -> String.split ~on:'|' x) raw_records
          )
      ) ;
      Bench.Test.create ~name:"Sort operation" (
        fun () ->
          ignore (
            List.sort ~cmp:(fun a b -> compare (List.nth_exn a 0) (List.nth_exn b 0)) records_to_sort
          )
      ) ;
      Bench.Test.create ~name:"Group operation" (
        fun () ->
          ignore (
            List.group ~break:(fun a b -> (List.nth_exn a 0) <> (List.nth_exn b 0)) records_to_group
          )
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

Estimated testing time 30s (3 benchmarks x 10s). Change using -quota SECS.
┌───────────────────────────┬──────────┬────────────┬──────────┬──────────┬────────────┐
│ Name                      │ Time/Run │    mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────────────┼──────────┼────────────┼──────────┼──────────┼────────────┤
│ Read Sort Group operation │  34.02ms │ 2_968.19kw │ 869.98kw │ 869.97kw │    100.00% │
│ Collapse Groups operation │   1.70ms │   315.75kw │  11.05kw │  11.05kw │      5.00% │
│ read_lines operation      │   5.35ms │   561.52kw │  82.88kw │  82.88kw │     15.73% │
└───────────────────────────┴──────────┴────────────┴──────────┴──────────┴────────────┘

Estimated testing time 40s (4 benchmarks x 10s). Change using -quota SECS.
┌──────────────────────────────┬──────────┬────────────┬──────────┬──────────┬────────────┐
│ Name                         │ Time/Run │    mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────────────┼──────────┼────────────┼──────────┼──────────┼────────────┤
│ Read Sort Group operation    │  32.32ms │ 2_968.19kw │ 868.31kw │ 868.31kw │    100.00% │
│ Collapse Groups operation    │   1.64ms │   315.75kw │  11.03kw │  11.03kw │      5.09% │
│ read_lines operation         │   5.46ms │   561.52kw │  82.66kw │  82.66kw │     16.90% │
│ String to List map operation │  13.13ms │   889.66kw │ 289.83kw │ 289.83kw │     40.63% │
└──────────────────────────────┴──────────┴────────────┴──────────┴──────────┴────────────┘

Estimated testing time 50s (5 benchmarks x 10s). Change using -quota SECS.
┌──────────────────────────────┬──────────┬────────────┬──────────┬──────────┬────────────┐
│ Name                         │ Time/Run │    mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────────────┼──────────┼────────────┼──────────┼──────────┼────────────┤
│ Read Sort Group operation    │  30.97ms │ 2_968.19kw │ 868.31kw │ 868.31kw │    100.00% │
│ Collapse Groups operation    │   1.68ms │   315.75kw │  11.02kw │  11.02kw │      5.41% │
│ read_lines operation         │   5.47ms │   561.52kw │  82.66kw │  82.66kw │     17.67% │
│ String to List map operation │  14.14ms │   889.66kw │ 286.67kw │ 286.67kw │     45.66% │
│ Sort operation               │   4.94ms │ 1_208.57kw │  35.44kw │  35.44kw │     15.95% │
└──────────────────────────────┴──────────┴────────────┴──────────┴──────────┴────────────┘

Estimated testing time 1m (6 benchmarks x 10s). Change using -quota SECS.
┌──────────────────────────────┬──────────┬────────────┬──────────┬──────────┬────────────┐
│ Name                         │ Time/Run │    mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────────────┼──────────┼────────────┼──────────┼──────────┼────────────┤
│ Read Sort Group operation    │  31.18ms │ 2_968.19kw │ 868.31kw │ 868.31kw │    100.00% │
│ Collapse Groups operation    │   1.64ms │   315.75kw │  11.03kw │  11.03kw │      5.26% │
│ read_lines operation         │   5.50ms │   561.52kw │  82.66kw │  82.66kw │     17.64% │
│ String to List map operation │  13.33ms │   889.66kw │ 289.83kw │ 289.83kw │     42.76% │
│ Sort operation               │   4.96ms │ 1_208.57kw │  35.56kw │  35.56kw │     15.91% │
│ Group operation              │   1.18ms │   308.43kw │  14.55kw │  14.55kw │      3.80% │
└──────────────────────────────┴──────────┴────────────┴──────────┴──────────┴────────────┘

***********************************)
