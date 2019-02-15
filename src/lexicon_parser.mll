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
      let trad = Array.of_list @@ String.split_on_char '/' trad in
      let trad =
        Array.map (fun t -> p_trad (Buffer.create 42) [] @@ Lexing.from_string t) trad
      in
      p_lang ((lang, trad) :: acc) lexbuf
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
      |> Array.of_list
    }
