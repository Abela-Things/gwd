open Jingoo

let template_dir = ref ""
let dev_version = ref false

let mk_env () =
  { Jg_types.autoescape = false
  ; template_dirs = [ !template_dir ]
  ; filters = []
  ; extensions = []
  ; strict_mode = true
  }

let open_templ file =
  let file = !template_dir ^ Filename.dir_sep ^ file in
  let ch = open_in_bin @@ file in
  let ast : Jg_types.statement list = Marshal.from_channel ch in
  close_in ch ;
  ast

let interp_templ ~models ast =
  let env = mk_env () in
  let output x = Wserver.print_string @@ Jg_runtime.string_of_tvalue x in
  let ctx = Jg_interp.init_context ~env ~models ~output () in
  let ast = Jg_interp.import_macros env ctx ast in
  ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast

let render_compiled ~file ~models =
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
    let ast = Jg_ast_optimize.inline_include env ast in
    let ast = Jg_interp.replace_blocks ast in
    let ast = Marshaler_lib.inline_const ast in
    let ast = Jg_ast_optimize.dead_code_elimination ast in
    let output x = Wserver.printf "%s" @@ Jg_runtime.string_of_tvalue x in
    let ctx = Jg_interp.init_context ~env ~models ~output () in
    let ast = Jg_interp.import_macros env ctx ast in
    ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast
  with e ->
    Wserver.printf "%s\n\n" @@ Printexc.to_string e ;
    Wserver.printf "%s" @@ Printexc.get_backtrace ()

let render ~conf ~file ~models =
  if !dev_version
  then render_jingoo ~file:(file ^ ".html.jingoo") ~models
  else render_compiled ~file:(file ^ ".html.jingoo." ^ conf.Geneweb.Config.lang) ~models
