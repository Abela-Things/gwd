open OUnit2
open Jingoo
open Jg_types
open Jg_runtime
open Geneweb
open Def
open Data

let field = Jg_runtime.jg_obj_lookup

let assert_equal_tvalue =
  let cmp a b = jg_eq_eq a b = Tbool true in
  assert_equal ~cmp ~printer:Jg_types.show_tvalue

let dmy ?(prec = Sure) day month year =
  { year ; month ; day ; delta = 0 ; prec }

let dmy2 day2 month2 year2 = { day2 ; month2 ; year2 ; delta2 = 0 }

let date d = mk_date (Dgreg (d, Dgregorian) )

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
  test 1 (dmy 2 7 1988) (dmy 1 7 1988)

let suite =
  "test_data" >:::
  [ "test_mk_date" >:: test_mk_date
  ; "test_date_compare" >:: test_date_compare
  ]
