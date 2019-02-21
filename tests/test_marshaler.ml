open OUnit2
open Jingoo
open Jg_types

let test_concat _ =
  assert_equal ~printer:Jg_types.show_ast [ TextStatement "foobaraz" ] @@
  Marshaler_lib.concat [ Statements [ TextStatement "foo" ]
                       ; ExpandStatement (LiteralExpr (Tstr "bar") )
                       ; TextStatement "baz"
                       ]

let suite =
  "test_marshaler" >:::
  [ "test_concat" >:: test_concat
  ]
