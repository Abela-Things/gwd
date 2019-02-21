open Jingoo
open Jg_types
open Marshaler_lib

let marshal verbose env file =
  inline_const_cnt := 0 ;
  inline_trans_cnt := 0 ;
  preapply_cnt := 0 ;
  concat_cnt := 0 ;
  flatten_cnt := 0 ;
  if verbose then print_string @@ "Marshaling " ^ file ^ ":" ;
  let file_in = file in
  let ch_in = open_in file_in in
  let lexbuf = Lexing.from_channel ch_in in
  Jg_lexer.reset_context () ;
  Jg_lexer.init_lexer_pos (Some file) lexbuf ;
  let ast =
    try Jg_parser.input Jg_lexer.main lexbuf
    with e -> raise @@ SyntaxError (Jg_utils.get_parser_error e lexbuf)
  in
  close_in ch_in ;
  let ast =
    (* General optimizations *)
    Jg_interp.unfold_extends env ast
    |> Jg_ast_optimize.inline_include env
    |> Jg_interp.replace_blocks
    |> inline_const
    |> Jg_ast_optimize.dead_code_elimination
  in
  let de, en, es, fi, fr, it, nl, no, pt, sv =
    Lazy.force Trans.de_en_es_fi_fr_it_nl_no_pt_sv
  in
  List.iter
    (fun (lang, trans) ->
       let ast =
         (* Other optimizations *)
         inline_trans trans ast
         |> preapply
         |> flatten
         |> concat
       in
       let file_out = file ^ "." ^ lang in
       let ch_out = open_out_bin file_out in
       Marshal.to_channel ch_out ast [] ;
       if verbose then print_string @@ " " ^ lang ;
       close_out ch_out)
    [ ( "de", de)
    ; ( "en", en)
    ; ( "es", es)
    ; ( "fi", fi)
    ; ( "fr", fr)
    ; ( "it", it)
    ; ( "nl", nl)
    ; ( "no", no)
    ; ( "pt", pt)
    ; ( "sv", sv)
    ] ;
  if verbose then begin
    print_endline " DONE!" ;
    print_endline @@ Printf.sprintf
      "inline_const: %d / inline_trans: %d / preapply: %d / concat: %d / flatten: %d"
      !inline_const_cnt
      !inline_trans_cnt
      !preapply_cnt
      !concat_cnt
      !flatten_cnt
  end

let ls dir filter =
  List.filter filter @@
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
      Sys.readdir f
      |> Array.to_list
      |> List.rev_map (Filename.concat f)
      |> List.rev_append fs
      |> loop result
    | f::fs -> loop (f :: result) fs
    | []    -> result
  in
  loop [] [dir]

let compile_dir verbose ext dir =
  let env = { Jg_types.autoescape = false
            ; template_dirs = [ dir ]
            ; filters = []
            ; extensions = []
            ; strict_mode = false }
  in
  let files = ls dir (fun f -> Filename.check_suffix f ext) in
  List.iter (marshal verbose env) files

let () =
  Printexc.record_backtrace true ;
  try
    let usage =
      "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
    in
    let dir = ref "." in
    let verbose = ref true in
    let ext = ref ".html.jingoo" in
    let speclist =
      [ ("--dir", Arg.Set_string dir, " Set the template dir (default is '.')")
      ; ("--quiet", Arg.Clear verbose, " Make it quiet (no output on stdout)")
      ; ( "--file-extension", Arg.Set_string ext
        , " Filter on file extension (default is .html.jingoo)")
      ; ("--lexicon", Arg.String (fun s -> Trans.lexicon_files := String.split_on_char ',' s)
        , " Files to use in order to inline translations (separated by comma)")
      ]
    in
    let anonfun s = raise (Arg.Bad s) in
    Arg.parse speclist anonfun usage ;
    print_endline @@ !dir ;
    compile_dir !verbose !ext !dir
  with _ -> Printexc.print_backtrace stdout

