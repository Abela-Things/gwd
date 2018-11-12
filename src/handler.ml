(* TODO check "unsafe" stuff *)
open Geneweb
open Config

let restricted_wizard fn self conf base =
  if conf.wizard then fn self conf base
  else self.RequestHandler.incorrect_request self conf base

let handler =
  let open RequestHandler in
  { defaultHandler
    with

      fallback = begin fun mode -> restricted_wizard @@ fun _self conf base ->
        match mode with
        | "SANDBOX" ->
          Interp.render_jingoo ~file:"sandbox.jingoo" ~models:(Data.sandbox conf base)
        | _ -> Wserver.printf "Error: unknown mode \"%s\"" mode
      end

  }

