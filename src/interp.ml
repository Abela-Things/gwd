open Jingoo

let template_dir = ref ""

let mk_env () =
  { Jg_types.autoescape = false
  ; template_dirs = [ !template_dir ]
  ; filters = []
  ; extensions = []
  ; strict_mode = true
  }

let open_templ file =
  let file = Filename.concat !template_dir file in
  let ch = open_in_bin @@ file in
  try
    let ast : Jg_types.statement list = Marshal.from_channel ch in
    close_in ch ;
    ast
  with _ -> close_in ch ; []

let interp_templ ~models ast =
  Printexc.record_backtrace true ;
  try
    let env = mk_env () in
    let output x = Wserver.printf "%s" @@ Jg_runtime.string_of_tvalue x in
    let ctx = Jg_interp.init_context ~env ~models ~output () in
    let ast = Jg_interp.import_macros env ctx ast in
    ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast
  with e ->
    Printexc.print_backtrace stdout ;
    raise e

let render ~file ~models =
  interp_templ ~models (open_templ file)

let render_jingoo ~file ~models =
  Printexc.record_backtrace true ;
  try
    let file = Filename.concat !template_dir file in
    let ch_in = open_in file in
    let lexbuf = Lexing.from_channel ch_in in
    Jg_lexer.reset_context () ;
    Jg_lexer.init_lexer_pos (Some file) lexbuf ;
    let ast =
      try Jg_parser.input Jg_lexer.main lexbuf
      with e -> failwith (Jg_utils.get_parser_error e lexbuf)
    in
    close_in ch_in ;
    let env = mk_env () in
    let ast = Jg_interp.unfold_extends env ast in
    let ast = Jg_interp.inline_include env ast in
    let ast = Jg_interp.replace_blocks ast in
    let output x = Wserver.printf "%s" @@ Jg_runtime.string_of_tvalue x in
    let ctx = Jg_interp.init_context ~env ~models ~output () in
    let ast = Jg_interp.import_macros env ctx ast in
    ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast
  with e ->
    Wserver.printf "%s\n\n" @@ Printexc.to_string e ;
    Wserver.printf "%s" @@ Printexc.get_backtrace ()
