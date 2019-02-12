open Jingoo
open Jg_types

let kwargs_args =
  let rec loop kwargs args = function
    | [] -> (kwargs, args)
    | (Some kw, LiteralExpr e) :: tl -> loop (kwargs @ [ (kw, e) ]) args tl
    | (None, LiteralExpr e) :: tl -> loop kwargs (args @ [e]) tl
    | _ -> raise Not_found
  in
  loop [] []

let inline_const stmts =
  let flag = ref true in
  let open Jg_ast_mapper in
  let ht = Hashtbl.create 128 in
  let statement self = function
    | SetStatement (SetExpr [ IdentExpr i ], ApplyExpr (IdentExpr "CONST", [ None, LiteralExpr e ]) ) ->
      Hashtbl.add ht i e ;
      flag := true ;
      Statements []
    | s -> default_mapper.statement self s
  in
  let expression self = function
    | IdentExpr i when Hashtbl.mem ht i -> LiteralExpr (Hashtbl.find ht i)
    | e -> default_mapper.expression self e
  in
  let mapper = { default_mapper with statement ; expression } in
  let rec loop ast = if !flag then (flag := false ; loop (mapper.ast mapper stmts)) else ast in
  loop stmts

let preapply stmts =
  let expression self = function
    | ApplyExpr (IdentExpr n , args) as e
      when List.for_all (function (_, LiteralExpr _) -> true | _ -> false) args
        && Array.exists (fun (n', _) -> n' = n) Jg_runtime.std_filters
      ->
      let kwargs, args = kwargs_args args in
      let rec loop i =
        let (n', fn) = Array.get Jg_runtime.std_filters i in
        if n = n'
        then match Jg_runtime.jg_apply ~kwargs fn args with
          | Tfun _ -> Jg_ast_mapper.default_mapper.expression self e
          | x -> LiteralExpr x
        else loop (i + 1)
      in loop 0
    | e -> Jg_ast_mapper.default_mapper.expression self e
  in
  let mapper = { Jg_ast_mapper.default_mapper with expression } in
  mapper.ast mapper stmts

(* TODO: detect string concatenation *)
let concat stmts =
  let flush str acc =
    if str = [] then acc
    else
      let s = String.concat "" (List.rev str) in
      let s = Str.global_replace (Str.regexp "[\n ]+") " " s in
      TextStatement s :: acc
  in
  let rec loop str acc = function
    | [] -> List.rev (flush str acc)
    | ExpandStatement (LiteralExpr (Tstr s)) :: tl
    | TextStatement s :: tl -> loop (s :: str) acc tl
    | IfStatement br :: tl ->
      cont str acc tl @@
      IfStatement (List.map (fun (e, stmts) -> e, loop [] [] stmts) br)
    | ForStatement (binds, e, stmts) :: tl ->
      cont str acc tl @@
      ForStatement (binds, e, loop [] [] stmts)
    | MacroStatement (exp, args, stmts) :: tl ->
      cont str acc tl @@
      MacroStatement (exp, args, loop [] [] stmts)
    | Statements stmts :: tl ->
      cont str acc tl @@
      Statements (loop [] [] stmts)
    | BlockStatement (e, stmts) :: tl ->
      cont str acc tl @@
      BlockStatement (e, loop [] [] stmts)
    | SwitchStatement (e, br) :: tl ->
      cont str acc tl @@
      SwitchStatement (e, List.map (fun (e, stmts) -> e, loop [] [] stmts) br)
    | s :: tl -> loop [] (s :: flush str acc) tl
  and cont str acc tl s =
      loop [] (s :: flush str acc) tl
  in
  loop [] [] stmts

let clean stmts =
  let rec loop acc = function
    | [] -> List.rev acc
    | Statements [] :: tl -> loop acc tl
    | hd :: tl -> loop (hd :: acc) tl
  in
  loop [] stmts

let inline_trans fn stmts =
  let open Jg_ast_mapper in
  let expression self = function
    | ApplyExpr (IdentExpr "trans", args) as e ->
      begin
        try
          let kwargs, args = kwargs_args args in
          match args with
          | [ Tint i ; Tstr s ] ->
            LiteralExpr (fn i kwargs s)
          | [ Tstr s ] ->
            let e =
              try (fn 0 kwargs s)
              with Not_found -> Tstr (Printf.sprintf "{{%s|trans}}" @@ Data.stringify s)
            in
            LiteralExpr e
          | _ ->
            default_mapper.expression self e
        with
        | Not_found -> default_mapper.expression self e
      end
    | e -> default_mapper.expression self e
  in
  let mapper = { default_mapper with expression } in
  mapper.ast mapper stmts

let marshal verbose env file =
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
  List.iter
    (fun (lang, trans) ->
       let ast =
         (* Other optimizations *)
         inline_trans trans ast
         |> preapply
         |> concat
         |> clean
       in
       let file_out = file ^ "." ^ lang in
       let ch_out = open_out_bin file_out in
       Marshal.to_channel ch_out ast [] ;
       if verbose then print_string @@ " " ^ lang ;
       close_out ch_out)
    [ ( "de", Trans.de)
    ; ( "en", Trans.en)
    ; ( "es", Trans.es)
    ; ( "fi", Trans.fi)
    ; ( "fr", Trans.fr)
    ; ( "it", Trans.it)
    ; ( "nl", Trans.nl)
    ; ( "no", Trans.no)
    ; ( "pt", Trans.pt)
    ; ( "sv", Trans.sv)
    ] ;
  if verbose then print_endline " DONE!"

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
    ]
  in
  let anonfun s = raise (Arg.Bad s) in
  Arg.parse speclist anonfun usage ;
  print_endline @@ !dir ;
  compile_dir !verbose !ext !dir
