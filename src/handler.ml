open Geneweb
open Config

let restricted_wizard fn self conf base =
  if conf.wizard then fn self conf base
  else self.RequestHandler.incorrect_request self conf base

let with_person self conf base fn =
  match Util.find_person_in_env conf base "" with
  | Some p -> fn p
  | None -> self.RequestHandler.very_unknown self conf base

let handler =
  let open RequestHandler in
  { defaultHandler with

    fallback = begin fun mode -> fun self conf base ->
      match mode with

      | "SANDBOX" ->
        restricted_wizard
          (fun _self conf base ->
             Interp.render_jingoo
               ~file:"sandbox.jingoo" ~models:(Data.sandbox conf base) )
          self conf base

      | "TIMELINE" ->
        with_person self conf base (fun p ->
            let root = Data.unsafe_mk_person conf base p in
            let models = ("root", root) :: Data.default_env conf base in
            Interp.render_jingoo ~file:"timeline.html.jingoo" ~models
          )

      | _ -> self.RequestHandler.incorrect_request self conf base
    end

  }
