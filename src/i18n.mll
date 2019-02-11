{

type i18n_expr =
  | Arg of string
  | Str of string

let flush buffer acc =
  let acc = match String.escaped (Buffer.contents buffer) with
    | "" -> acc
    | x -> Str x :: acc in
  Buffer.clear buffer ;
  acc

let to_ocaml_var_name s =
  Bytes.init (String.length s)
    (fun i -> match String.get s i with
       | 'a'..'z' | '0'..'1' | 'A'..'Z' as c -> c
       | _ -> '_')
  |> Bytes.unsafe_to_string

let default_lang = "en"
let langs = [ "de" ; "en" ; "es" ; "fi" ; "fr" ; "it" ; "nl" ; "no" ; "pt" ; "sv" ]

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let num = ['0'-'9']

let id = (lower | ['_']) (lower | upper | num | ['_'])*

let line = [^ '\n' ]+
let eol = '\n'

rule p_main acc = parse
  | ' '+ (line as t) eol
      { p_main ((t, p_lang [] lexbuf) :: acc) lexbuf }
  | _
      { p_main acc lexbuf }
  | eof { acc }

and p_lang acc = parse
  | ((lower | '-' )+ as lang) ':' ' '? (line as trad) eol {
      if List.mem lang langs then
        let trad = String.split_on_char '/' trad in
        let trad =
          List.map (fun t -> p_trad (Buffer.create 42) [] @@ Lexing.from_string t) trad
        in
        p_lang ((lang, trad) :: acc) lexbuf
      else p_lang acc lexbuf
    }
  | "" { acc }

and p_trad buffer acc = parse
  | '%' (num+ as n) {
      let acc = flush buffer acc in
      p_trad buffer (Arg ("_" ^ n) :: acc) lexbuf
    }
  | '%' (lower+ as n) {
      let acc = flush buffer acc in
      p_trad buffer (Arg n :: acc) lexbuf
    }
  | _ as c {
      Buffer.add_char buffer c ;
      p_trad buffer acc lexbuf
    }
  | eof {
      let base s =
        let rec loop i =
          if i = 0 then ""
          else if s.[i] >= '0' && s.[i] <= '9' then loop (i - 1)
          else String.sub s 0 (i + 1)
        in loop (String.length s - 1)
      in
      let occ s list = List.fold_left (fun sum -> function Arg x when base x = s -> sum + 1 | _ -> sum) 1 list in
      let rec loop acc = function
        | Arg hd :: tl ->
          let occ = occ hd tl in
          if occ = 1 && not (List.exists (function Arg s -> base s = hd | _ -> false) acc)
          then loop (Arg hd :: acc) tl
          else loop (Arg (hd ^ string_of_int occ) :: acc) tl
        | hd :: tl -> loop (hd :: acc) tl
        | [] -> acc
      in
      loop [] (flush buffer acc)
    }

{

let print_ocaml_header fmt =
  Format.pp_print_string fmt "open Jingoo\nopen Jg_types\nopen Jg_runtime\n" ;
  Format.pp_print_string fmt
    {|let fast_concat = function
      | [] -> ""
      | l ->
        let b =
          Bytes.create (List.fold_left (fun acc s -> String.length s + acc) 0 l)
        in
        ignore @@ List.fold_left
          (fun pos s ->
            let len = String.length s in
            Bytes.unsafe_blit (Bytes.unsafe_of_string s) 0 b pos len ;
            pos + len)
          0 l ;
        Bytes.unsafe_to_string b
     |}

let print_list fmt format sep printer =
  Format.fprintf fmt format
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt sep)
       printer)

let print_list_i fmt format sep printer =
  let i = ref 0 in
  Format.fprintf fmt format
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> incr i ; Format.pp_print_string fmt sep)
       (fun fmt x -> printer fmt !i x) )


let print_part fmt =
  function
  | Str s -> Format.fprintf fmt "\"%s\"" s
  | Arg v -> Format.fprintf fmt "string_of_tvalue %s" v


let args line =
  List.sort_uniq compare @@
  List.fold_left (fun acc list ->
      List.fold_left
        (fun acc -> function Arg x -> x :: acc | _ -> acc)
        acc list)
    [] line

let find_lang lang tr =
  try List.assoc lang tr with Not_found -> List.assoc default_lang tr

let print_lang fmt i lang tr =
  let line = find_lang lang tr in
  let args = args line in
  Format.fprintf fmt "let _%s_%i%s%s =\n" lang i
    (if List.length line > 1 then " nth" else "")
    (if args <> [] then " kwargs" else "") ;
  if args <> [] then
    print_list fmt "%a\n" "\n"
      (fun fmt x ->
         Format.fprintf fmt "let %s = List.assoc \"%s\" kwargs in" x x)
      args ;
  print_list fmt (if List.length line > 1 then "Tstr ([|%a|].(nth))\n" else "Tstr (%a)\n") ";"
    (fun fmt -> function
       | [ x ] -> print_part fmt x
       | trad -> print_list fmt "fast_concat [%a]" ";" print_part trad)
    line

let print_ocaml output key_value =
  print_list_i output "%a" "\n"
    (fun fmt i (_key, tr) ->
       print_list fmt "%a" "\n"
         (fun fmt lang -> print_lang fmt i lang tr)
         langs)
    key_value ;
  print_list output "%a" "\n"
    (fun fmt lang ->
       Format.fprintf fmt "let %s =\nfun nth kwargs x -> unbox_string x |> function\n" lang ;
       print_list_i fmt "%a" "\n"
         (fun fmt i (key, tr) ->
            let line = find_lang lang tr in
            Format.fprintf fmt "| \"%s\" -> _%s_%i%s%s" (String.escaped key) lang i
              (if List.length line > 1 then " nth" else "")
              (if args line <> [] then " kwargs" else "") )
         key_value ;
       Format.pp_print_string output "\n| _ -> raise Not_found\n"
    ) langs

let _ =
  let acc =
    List.fold_left
      (fun acc file ->
         let in_chan = open_in file in
         let lexbuf = Lexing.from_channel in_chan in
         try let acc = p_main acc lexbuf in close_in in_chan ; acc
         with Failure msg ->
           failwith (Printf.sprintf "%s line: %d" msg lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum))
      [] (String.split_on_char ',' Sys.argv.(1))
  in
  let out_chan = open_out Sys.argv.(2) in
  let key_values =
    let rec loop acc = function
      | [] -> acc
      | (key, trad) as hd :: tl ->
        let acc =
          let key = to_ocaml_var_name key in
          if List.exists (fun (k, _) -> to_ocaml_var_name k = key) acc
          || not (List.mem_assoc default_lang trad)
          then acc
          else hd :: acc
        in
        loop acc tl
    in
    loop [] acc
  in
  let output = Format.formatter_of_out_channel out_chan in
  print_ocaml_header output ;
  print_ocaml output key_values ;
  close_out out_chan
}
