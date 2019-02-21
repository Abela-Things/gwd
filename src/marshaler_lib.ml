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

let is_std_filter n =
  Array.exists (fun (n', _) -> n' = n) Jg_runtime.std_filters

let inline_const_cnt = ref 0

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
    | IdentExpr i when Hashtbl.mem ht i -> incr inline_const_cnt ; LiteralExpr (Hashtbl.find ht i)
    | e -> default_mapper.expression self e
  in
  let mapper = { default_mapper with statement ; expression } in
  let rec loop ast = if !flag then (flag := false ; loop (mapper.ast mapper ast)) else ast in
  loop stmts

let preapply_cnt = ref 0

let preapply stmts =
  let open Jg_ast_mapper in
  let local_variables : (string * string list) list ref = ref [("", [])] in
  let push_block name = local_variables := (name, []) :: !local_variables in
  let pop_block () = local_variables := List.tl !local_variables in
  let set_local x =
    let fst, snd = List.hd !local_variables in
    local_variables := (fst, x :: snd ) :: (List.tl !local_variables) in
  let is_local (x : string) = List.exists (fun (_, l) -> List.mem x l) !local_variables in
  let rec maybe_set = function
    | SetExpr set -> List.iter maybe_set set
    | IdentExpr id -> set_local id
    | _ -> () in
  let statement self = function
    | SetStatement (id, _) as s ->
      maybe_set id ;
      default_mapper.statement self s
    | ForStatement (id, _, _) as s ->
      push_block "" ;
      List.iter set_local id ;
      let s = default_mapper.statement self s in
      pop_block () ;
      s
    | FunctionStatement (id, args, _)
    | MacroStatement (id, args, _) as s ->
      push_block id ;
      set_local id ;
      List.iter (fun (i, _) -> set_local i) args ;
      let s = default_mapper.statement self s in
      pop_block () ;
      s
    | CallStatement(_, args, _, _) as s ->
      push_block "" ;
      List.iter (fun (i, _) -> set_local i) args ;
      let s = default_mapper.statement self s in
      pop_block () ;
      s
    | TextStatement (_)
    | ExpandStatement (_)
    | IfStatement (_)
    | SwitchStatement (_, _)
    | IncludeStatement (_, _)
    | RawIncludeStatement _
    | ExtendsStatement _
    | ImportStatement (_, _)
    | FromImportStatement (_, _)
    | BlockStatement (_, _)
    | FilterStatement (_, _)
    | WithStatement (_, _)
    | AutoEscapeStatement (_, _)
    | NamespaceStatement (_, _)
    | Statements (_)
      as s -> default_mapper.statement self s
  in
  let expression self = function
    | ApplyExpr (IdentExpr n , args) as e
      when List.for_all (function (_, LiteralExpr _) -> true | _ -> false) args
        && (not @@ is_local n) && is_std_filter n ->
      let kwargs, args = kwargs_args args in
      let rec loop i =
        let (n', fn) = Array.get Jg_runtime.std_filters i in
        if n = n'
        then match Jg_runtime.jg_apply ~kwargs fn args with
          | Tfun _ -> Jg_ast_mapper.default_mapper.expression self e
          | x -> incr preapply_cnt ; LiteralExpr x
        else loop (i + 1)
      in loop 0
    | e -> Jg_ast_mapper.default_mapper.expression self e
  in
  let mapper = { Jg_ast_mapper.default_mapper with statement ; expression } in
  mapper.ast mapper stmts

let concat_cnt = ref 0

let concat stmts =
  let flush str acc =
    if str = [] then acc
    else
      let () = concat_cnt := !concat_cnt + List.length str in
      let s = String.concat "" (List.rev str) in
      let s = Str.global_replace (Str.regexp "[\n ]+") " " s in
      TextStatement s :: acc
  in
  let rec loop str acc = function
    | [] -> List.rev (flush str acc)
    | ExpandStatement (LiteralExpr (Tstr s)) :: tl
    | TextStatement s :: tl -> loop (s :: str) acc tl
    | Statements stmts :: tl ->
      cont str acc tl @@
      Statements (loop [] [] stmts)
    | IfStatement br :: tl ->
      cont str acc tl @@
      IfStatement (List.map (fun (e, stmts) -> e, loop [] [] stmts) br)
    | ForStatement (binds, e, stmts) :: tl ->
      cont str acc tl @@
      ForStatement (binds, e, loop [] [] stmts)
    | MacroStatement (exp, args, stmts) :: tl ->
      cont str acc tl @@
      MacroStatement (exp, args, loop [] [] stmts)
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

let flatten_cnt = ref 0

let flatten stmts =
  let rec loop acc = function
    | [] -> List.rev acc
    | Statements stmts :: tl ->
      incr flatten_cnt ;
      loop (List.rev_append (loop [] stmts) acc) tl
    | hd :: tl -> loop (hd :: acc) tl
  in
  loop [] stmts

let inline_trans_cnt = ref 0

let inline_trans ht stmts =
  let open Jg_ast_mapper in
  let stringify s =
    Printf.sprintf (if String.contains s '\'' then "\"%s\"" else "'%s'") s
  in
  let fn ~kwargs s i =
    incr inline_trans_cnt ;
    LiteralExpr begin
      try (Hashtbl.find ht s) ?kwargs:(Some kwargs) i
      with Not_found -> Tstr (Printf.sprintf "{{%s|trans}}" @@ stringify s)
    end
  in
  let expression self = function
    | ApplyExpr (IdentExpr "trans", args) as e ->
      begin try
          let kwargs, args = kwargs_args args in
          match args with
          | [ Tint i ; Tstr s ] -> fn ~kwargs s i
          | [ Tstr s ] -> fn ~kwargs s 0
          | _ -> default_mapper.expression self e
        with Not_found -> default_mapper.expression self e
      end
    | e -> default_mapper.expression self e
  in
  let mapper = { default_mapper with expression } in
  mapper.ast mapper stmts
