open OUnit2
open Jingoo
open Jg_types
open Jg_runtime
open Geneweb
open Def
open Data
open Test_utils

let field = Jg_runtime.jg_obj_lookup

let assert_equal_tvalue =
  let cmp a b = jg_eq_eq a b = Tbool true in
  assert_equal ~cmp ~printer:Jg_types.show_tvalue

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
  let test exp a b =
    assert_equal_tvalue (Tint exp) (date_compare_aux (date a) (date b))
  in
  test 0 (dmy 2 7 1988) (dmy 2 7 1988) ;
  test (-1) (dmy 2 7 1988) (dmy 3 7 1988) ;
  test 1 (dmy 2 7 1988) (dmy 1 7 1988) ;
  test 1 (dmy ~prec:After 0 0 1892) (dmy 15 2 1892)

let suite =
  "test_data" >:::
  [ "test_mk_date" >:: test_mk_date
  ; "test_date_compare" >:: test_date_compare
  ]
