open Jingoo
open Jg_types

let marshal verbose env file =
  if verbose then print_endline @@ "Marshaling: " ^ file ;
  let file_in = file in
  let file_out = file ^ ".marshaled" in
  let ch_in = open_in file_in in
  let ch_out = open_out_bin file_out in
  let lexbuf = Lexing.from_channel ch_in in
  Jg_lexer.reset_context () ;
  Jg_lexer.init_lexer_pos (Some file) lexbuf ;
  let ast =
    try Jg_parser.input Jg_lexer.main lexbuf
    with e -> raise @@ SyntaxError (Jg_utils.get_parser_error e lexbuf)
  in
  let ast = Jg_interp.unfold_extends env ast in
  let ast = Jg_interp.inline_include env ast in
  let ast = Jg_interp.replace_blocks ast in
  Marshal.to_channel ch_out ast [] ;
  close_in ch_in ;
  close_out ch_out

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

let compile_dir dir =
  let env = { Jg_types.autoescape = false
            ; template_dirs = [ dir ]
            ; filters = []
            ; extensions = []
            ; strict_mode = false }
  in
  let files = ls dir (fun f -> Filename.extension f = ".jingoo") in
  List.iter (marshal true env) files

let () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let dir = ref "." in
  let speclist =
    [ ("-dir", Arg.String (fun x -> dir := x), " Set the template dir (default is '.')") ]
  in
  let anonfun s = raise (Arg.Bad s) in
  Arg.parse speclist anonfun usage ;
  print_endline @@ !dir ;
  compile_dir !dir
