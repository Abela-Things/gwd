open Geneweb
open Def
open Test_utils
open Benchmark

let bench_mk_date () =
  let bench list =
    let n = 9999999L in
    latency1 n ~name:"mk_date" (fun () -> Array.iter (fun d -> ignore (date d)) list) ()
  in
  bench
    [| dmy ~prec:After 0 0 1892
     ; dmy 15 2 1892
    |]

let suite = [ bench_mk_date ]

