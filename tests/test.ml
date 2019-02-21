open OUnit2

let () =
  run_test_tt_main ("gwd" >::: [ Test_data.suite
                               ; Test_marshaler.suite
                               ])
