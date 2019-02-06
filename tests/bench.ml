let _ =
  [ Bench_data.suite ]
  |> List.iter @@
  fun s -> List.iter (fun f -> Benchmark.tabulate @@ f ()) s
