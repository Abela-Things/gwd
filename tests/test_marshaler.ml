open OUnit2
open Jingoo
open Jg_types

let assert_equal = assert_equal ~printer:Jg_types.show_ast


let test_flatten _ =
  let test expected input = assert_equal expected (Marshaler_lib.flatten input) in
  test [] [ Statements [ Statements [] ] ; Statements [] ] ;
  test [ TextStatement "" ] [ Statements [ Statements [ TextStatement "" ] ] ; Statements [] ]

let test_concat _ =
  let test expected input = assert_equal expected (Marshaler_lib.concat input) in
  test [ TextStatement "foobarbaz" ] [ TextStatement "foo"
                                     ; ExpandStatement (LiteralExpr (Tstr "bar") )
                                     ; TextStatement "baz"
                                     ]

let test_flatten_concat _ =
  let test expected input =
    assert_equal expected (Marshaler_lib.concat @@ Marshaler_lib.flatten input)
  in
  test [ TextStatement "foobarbaz" ] [ Statements [ TextStatement "foo" ]
                                     ; ExpandStatement (LiteralExpr (Tstr "bar") )
                                     ; Statements []
                                     ; Statements [ TextStatement "baz" ]
                                     ]

let suite =
  "test_marshaler" >:::
  [ "test_flatten" >:: test_flatten
  ; "test_concat" >:: test_concat
  ; "test_flatten_concat" >:: test_flatten_concat
  ]
