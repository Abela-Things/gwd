open OUnit2
open Jingoo
open Jg_types
open Jg_runtime
open Geneweb
open Def
open Data
open Test_utils

let field = Jg_runtime.jg_obj_lookup

let assert_equal_tvalue ?(printer = Jg_types.show_tvalue) a b =
  let cmp a b = jg_eq_eq a b = Tbool true in
  assert_equal ~cmp ~printer a b

let test_mk_date _ctx =
  let rec test ({ year ; month ; day ; prec ; _ } as d) =
    let open Data in
    let d = date d in
    assert_equal_tvalue (Tint day) (field d "day") ;
    assert_equal_tvalue (Tint month) (field d "month") ;
    assert_equal_tvalue (Tint year) (field d "year") ;
    match prec with
    | OrYear d | YearInt d -> test (dmy d.day2 d.month2 d.year2)
    | _ -> ()
  in
  test (dmy 2 7 1988)

let test_date_compare _ctx =
  let test loc exp a b =
    let printer x = Printf.sprintf "%s -- %s" (Jg_types.show_tvalue x) loc in
    let assert_equal_tvalue = assert_equal_tvalue ~printer in
    assert_equal_tvalue (Tint exp) (date_compare_aux (date a) (date b)) ;
    assert_equal_tvalue (Tint (-exp)) (date_compare_aux (date b) (date a)) ;
    assert_equal_tvalue (Tint 0) (date_compare_aux (date a) (date a)) ;
    assert_equal_tvalue (Tint 0) (date_compare_aux (date b) (date b))
  in
  test __LOC__ 0 (dmy 2 7 1988) (dmy 2 7 1988) ;
  test __LOC__ (-1) (dmy 2 7 1988) (dmy 3 7 1988) ;
  test __LOC__ 1 (dmy 2 7 1988) (dmy 1 7 1988) ;
  test __LOC__ 1 (dmy ~prec:After 0 0 1892) (dmy 15 2 1892) ;
  test __LOC__ 1 (dmy ~prec:After 0 0 1892) (dmy ~prec:After 15 2 1892) ;
  test __LOC__ 1 (dmy ~prec:After 0 0 1892) (dmy ~prec:After 15 2 1892) ;
  test __LOC__ 0 (dmy 5 6 1996) (dmy 0 0 1996)

let test_date_compare_list _ctx =
  assert_equal
    [ (dmy 0 0 1990), 0
    ; (dmy ~prec:Before 0 0 1995), 1
    ; (dmy 1 6 1995), 2
    ; (dmy ~prec:After 0 0 1995), 3
    ; (dmy 5 4 1996), 4
    ; (dmy 0 0 1996), 5
    ; (dmy 5 6 1996), 6
    ; (dmy 0 0 1996), 7
    ; (dmy 0 0 1996), 8
    ; (dmy 0 0 1997), 9
    ]
    (let compare (a ,_) (b ,_) = unbox_int (date_compare_aux (date a) (date b)) in
     List.stable_sort compare
       [ (dmy ~prec:After 0 0 1995), 3
       ; (dmy 1 6 1995), 2
       ; (dmy 5 4 1996), 4
       ; (dmy 0 0 1996), 5
       ; (dmy 0 0 1997), 9
       ; (dmy 5 6 1996), 6
       ; (dmy 0 0 1990), 0
       ; (dmy 0 0 1996), 7
       ; (dmy ~prec:Before 0 0 1995), 1
       ; (dmy 0 0 1996), 8
       ]
    )

let test_json_encode _ctx =
  let str =
    {|Un évènement avec des "quotes" ou des 'single-quote', des \" et des \'|}
  in
  let expected =
    {|"Un évènement avec des \"quotes\" ou des 'single-quote', des \\\" et des \\'"|}
  in
  assert_equal_tvalue (Tstr expected) (Tstr (json_encode @@ Tstr str))

let suite =
  "test_data" >:::
  [ "test_mk_date" >:: test_mk_date
  ; "test_date_compare" >:: test_date_compare
  ; "test_date_compare_list" >:: test_date_compare_list
  ; "test_json_encode" >:: test_json_encode
  ]
