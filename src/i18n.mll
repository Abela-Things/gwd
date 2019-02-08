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
          List.map (fun t -> p_trad 1 (Buffer.create 42) [] @@ Lexing.from_string t) trad
        in
        p_lang ((lang, trad) :: acc) lexbuf
      else p_lang acc lexbuf
    }
  | "" { acc }

and p_trad cnt buffer acc = parse
  | '%' (num+ as n) {
      let acc = flush buffer acc in
      p_trad cnt buffer (Arg ("_" ^ n) :: acc) lexbuf
    }
  | '%' lower+ {
      let acc = flush buffer acc in
      p_trad (cnt + 1) buffer (Arg ("_" ^ string_of_int cnt)  :: acc) lexbuf
    }
  | _ as c {
      Buffer.add_char buffer c ;
      p_trad cnt buffer acc lexbuf
    }
  | eof {
      List.rev (flush buffer acc)
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

let print_lang fmt key lang tr =
  let line = find_lang lang tr in
  let args = args line in
  Format.fprintf fmt "let _%s_%s %s=\n" (to_ocaml_var_name key) lang
    (if args <> [] || List.length line > 1 then "kwargs " else "") ;
  if args <> [] then
    print_list fmt "%a\n" "\n"
      (fun fmt x ->
         Format.fprintf fmt "let %s = List.assoc \"%s\" kwargs in" x x)
      args ;
  if List.length line > 1 then
    Format.pp_print_string fmt
      "let nth = try unbox_int @@ List.assoc \"nth\" kwargs with Not_found -> 0 in\n" ;
  print_list fmt (if List.length line > 1 then "Tstr ([|%a|].(nth))\n" else "Tstr (%a)\n") ";"
    (fun fmt -> function
       | [ x ] -> print_part fmt x
       | trad -> print_list fmt "fast_concat [%a]" ";" print_part trad)
    line

let print_ocaml output key_value =
  print_list output "%a" "\n"
    (fun fmt (key, tr) ->
       print_list fmt "%a" "\n"
         (fun fmt lang -> print_lang fmt key lang tr)
         langs)
    key_value ;
  print_list output "%a" "\n"
    (fun fmt lang ->
       Format.fprintf fmt "let %s =\nfun ?(kwargs=[]) x -> unbox_string x |> function\n" lang ;
       print_list fmt "%a" "\n"
         (fun fmt (key, tr) ->
            Format.fprintf fmt "| \"%s\" -> _%s_%s%s" (String.escaped key) (to_ocaml_var_name key) lang
              (let line = find_lang lang tr in
               if args line <> [] || List.length line > 1 then " kwargs" else "") )
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
