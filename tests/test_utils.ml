open Def
open Gwd_lib.Data

let dmy ?(prec = Sure) day month year =
  { year ; month ; day ; delta = 0 ; prec }

let dmy2 day2 month2 year2 = { day2 ; month2 ; year2 ; delta2 = 0 }

let date d = mk_date (Dgreg (d, Dgregorian) )

