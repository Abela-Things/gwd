open Jingoo

let template_dir = ref ""

let open_templ file =
  let file = Filename.concat !template_dir file in
  let ch = open_in_bin @@ file ^ ".jinja2.marshaled" in
  try
    let ast : Jg_types.statement list = Marshal.from_channel ch in
    close_in ch ;
    ast
  with _ -> close_in ch ; []

let interp_templ ~models ast =
  Printexc.record_backtrace true ;
  try
    let env = { Jg_types.autoescape = false
              ; template_dirs = [ !template_dir ]
              ; filters = []
              ; extensions = []
              ; strict_mode = true
              }
    in
    let output x = Wserver.printf "%s" @@ Jg_runtime.string_of_tvalue x in
    let ctx = Jg_interp.init_context ~env ~models ~output () in
    let ast = Jg_interp.import_macros env ctx ast in
    ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast
  with e ->
    Printexc.print_backtrace stdout ;
    raise e

let render ~file ~models =
  interp_templ ~models (open_templ file)
