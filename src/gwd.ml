module GW = Geneweb

module Request = GW.Request.Make (struct let handler = Handler.handler end)
module Main = GW.GwDaemon.Make (Wserver) (Request)

let speclist =
  [ ("-td", Arg.Set_string Interp.template_dir, " Set the template directory.")
  ; ("--dev", Arg.Set Interp.dev_version, " Do not use compiled templates.")
  ]

let _ = Main.run ~speclist ()
