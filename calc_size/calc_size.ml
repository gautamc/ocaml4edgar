open Sys;;
open Printf;;
open Str;;
open Unix;;


let input_chan = open_in "/data/yacct/edgar/daily-index/2014_01/master.20140110.idx" in
  let rec process_csv ?ix f =
    match
      try Some (input_line input_chan) with
      | _ -> close_in input_chan; None
    with
    | None -> ""
    | Some x ->
      (
        let ix = match ix with
          | None -> 0
          | Some ix -> ix
        in
          f x ix; process_csv ~ix:(ix+1) f
      )
  in
    process_csv (
      fun x ix ->
        if ix > 6 && ix < 8 then
          let fields = Str.split (regexp "|") x in
            printf "%d - %d\n" ix (List.length fields)
    )
