open Geneweb
open Gwdb
open Config
open Jingoo
open Jg_types

type alphabetic = Str of string | Chr of char | Empty

let unaccent trimmed s i0 len =
  let rec loop i =
    if i < len then match

        match Char.code @@ String.unsafe_get s i with

        (* A..Z *)
        |0x41|0x42|0x43|0x44|0x45|0x46|0x47|0x48|0x49|0x4A|0x4B|0x4C|0x4D|0x4E|0x4F
        |0x50|0x51|0x52|0x53|0x54|0x55|0x56|0x57|0x58|0x59|0x5A
        as c -> Chr (Char.unsafe_chr @@ c + 32), i, succ i

        (* a..z *)
        |0x61|0x62|0x63|0x64|0x65|0x66|0x67|0x68|0x69|0x6A|0x6B|0x6C|0x6D|0x6E|0x6F
        |0x70|0x71|0x72|0x73|0x74|0x75|0x76|0x77|0x78|0x79|0x7A

        (* 0..9 *)
        |0x30|0x31|0x32|0x33|0x34|0x35|0x36|0x37|0x38|0x39
        as c -> Chr (Char.unsafe_chr c), i, succ i

        (* '-' | ' ' | '\'' *)
        | 0x2D | 0x20 | 0x27 as c ->
          (if trimmed then Chr (Char.unsafe_chr c) else Empty), i, succ i

        | _ ->

          Unidecode.decode
            (fun n -> function "" ->Empty, i, n | s -> Str (String.lowercase_ascii s), i, n)
            (fun n c -> match Char.lowercase_ascii c with
               | 'a'..'z' | '0'..'9' as c -> Chr c, i, n
               | '-' | ' ' | '\'' as c -> (if trimmed then Chr c else Empty), i, n
               | _ -> Empty, i, n)
            (fun n -> Empty, i, n)
            s i len
      with
      | Empty, _, n -> loop n
      | x, i, n -> x, i, n
    else Empty, i0, len
  in
  loop i0

(* See BatUTF8.look source (from batteries) *)
let cp s i =
  Uchar.of_int @@
  let n = Char.code (String.unsafe_get s i) in
  if n < 0x80 then n else
  if n <= 0xdf then
    (n - 0xc0) lsl 6 lor (0x7f land (Char.code (String.unsafe_get s (i + 1))))
  else if n <= 0xef then
    let n' = n - 0xe0 in
    let m = Char.code (String.unsafe_get s (i + 1)) in
    let n' = n' lsl 6 lor (0x7f land m) in
    let m = Char.code (String.unsafe_get s (i + 2)) in
    n' lsl 6 lor (0x7f land m)
  else
    let n' = n - 0xf0 in
    let m = Char.code (String.unsafe_get s (i + 1)) in
    let n' = n' lsl 6 lor (0x7f land m) in
    let m = Char.code (String.unsafe_get s (i + 2)) in
    let n' = n' lsl 6 lor (0x7f land m) in
    let m = Char.code (String.unsafe_get s (i + 3)) in
    n' lsl 6 lor (0x7f land m)

let cmp_substring s1 i1 j1 s2 i2 j2 =
  let l1 = j1 - i1 in
  let l2 = j2 - i2 in
  if l1 = 1 && l2 = 1
  then
    (* Optimize ASCII characters comparison *)
    Stdlib.compare
      (Char.lowercase_ascii @@ String.get s1 i1)
      (Char.lowercase_ascii @@ String.get s2 i2)
  else
    let c1 = cp s1 i1 in
    let c2 = cp s2 i2 in
    let c1 = match Uucp.Case.Fold.fold c1 with `Self -> [c1] | `Uchars us -> us in
    let c2 = match Uucp.Case.Fold.fold c2 with `Self -> [c2] | `Uchars us -> us in
    compare c1 c2

let alphabetic n1 n2 =
  let trimmed1 = ref false in
  let trimmed2 = ref false in
  let rec loop i1 i2 =
    if i1 >= String.length n1 && i2 >= String.length n2 then i1 - i2
    else if i1 >= String.length n1 then -1
    else if i2 >= String.length n2 then 1
    else
      let (c1, i1, ii1) = unaccent !trimmed1 n1 i1 (String.length n1) in
      let (c2, i2, ii2) = unaccent !trimmed2 n2 i2 (String.length n2) in
      let () = trimmed1 := true in
      let () = trimmed2 := true in
      let cmp_aux = function
        | 0 -> begin match cmp_substring n1 i1 ii1 n2 i2 ii2 with
            | 0 -> loop ii1 ii2
            | x -> x
          end
        | x -> x
      in
      match c1, c2 with
      | Str s1, Str s2 -> cmp_aux (Stdlib.compare s1 s2)
      | Chr c1, Chr c2 -> cmp_aux (Stdlib.compare c1 c2)
      | Empty, Empty -> cmp_aux 0
      | (Str _ | Chr _), Empty -> -1
      | Empty, (Str _ | Chr _) -> 1
      | Str s1, Chr c2 -> begin
          match Stdlib.compare (String.unsafe_get s1 0) c2 with
          | 0 -> 1
          | x -> x
        end
      | Chr c1, Str s2 -> begin
          match Stdlib.compare c1 (String.unsafe_get s2 0) with
          | 0 -> -1
          | x -> x
        end
  in
  if n1 = n2 then 0 else loop 0 0

let lower_fst =
  let b = Buffer.create 6 in
  fun s ->
    Buffer.reset b ;
    let _, i0, _ = unaccent false s 0 (String.length s) in
    let u = cp s i0 in
    begin match Uucp.Case.Map.to_lower u with
      | `Self -> Uutf.Buffer.add_utf_8 b u
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
    end ;
    Buffer.contents b

let list_ind_file conf =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb")) @@
  if conf.wizard || conf.friend then "cache_list_ind_friend" else "cache_list_ind_visitor"

let is_cache_iper_inorder_uptodate conf base =
  let cache_path = list_ind_file conf in
  try
    let cache_stat = Unix.stat (cache_path) in
    let cache_timeof_modif = cache_stat.Unix.st_mtime in
    let base_timeof_modif = Gwdb.date_of_last_change base in
    (base_timeof_modif < cache_timeof_modif)
  with
    Unix.Unix_error _ -> false

let build_cache_iper_inorder conf base =
  let module PerSet =
    Set.Make (struct
      type t = person
      let compare p1 p2 =
        match
          alphabetic
            (Util.name_key base @@ Ezgw.Person.surname base p1)
            (Util.name_key base @@ Ezgw.Person.surname base p2)
        with
        | 0 -> begin
            match alphabetic (Ezgw.Person.first_name base p1) (Ezgw.Person.first_name base p2) with
            | 0 -> compare (Ezgw.Person.occ p1) (Ezgw.Person.occ p2)
            | x -> x
          end
        | x -> x
    end)
  in
  Gwdb.load_persons_array base ;
  Gwdb.load_strings_array base ;
  let set =
    Gwdb.Collection.fold begin fun set p ->
      (* FIXME: stop checking is_empty_name when possible *)
      if (Util.is_empty_name p) || not (Util.authorized_age conf base p) then set
      else PerSet.add p set
    end PerSet.empty (Gwdb.persons base)
  in
  Gwdb.clear_persons_array base ;
  Gwdb.clear_strings_array base ;
  let cache_filename = list_ind_file conf in
  let cnt = PerSet.cardinal set in
  let _, letters =
    PerSet.fold begin fun x (idx, acc) ->
      let c = lower_fst @@ Util.name_key base @@ Ezgw.Person.surname base x in
      if List.mem_assoc c acc then (succ idx, acc) else (succ idx, (c, idx) :: acc)
    end set (0, [])
  in
  Geneweb.Lock.control
    (cache_filename ^ ".lock") true ~onerror:Lock.print_try_again @@ fun () ->
  let letters = List.rev letters in
  let oc = Secure.open_out_bin cache_filename in
  output_binary_int oc cnt;
  output_value oc letters;
  let m1 = pos_out oc in
  PerSet.iter (fun _ -> output_binary_int oc 0) set ;
  let m2 = pos_out oc in
  ignore @@
  PerSet.fold begin fun x (m1, m2) ->
    seek_out oc m1 ;
    output_binary_int oc m2 ;
    let m1 = pos_out oc in
    seek_out oc m2 ;
    output_value oc (get_iper x) ;
    let m2 = pos_out oc in
    (m1, m2)
  end set (m1, m2) ;
  close_out oc

let read_cache_iper_inorder conf page page_size letter =
  let cache_filename = list_ind_file conf in
  let ic = Secure.open_in_bin cache_filename in
  let person_count = input_binary_int ic in
  let page_count = person_count / page_size + if person_count mod page_size == 0 then 0 else 1 in
  let first_letters : (string * int) list = input_value ic in
  let page =
    match letter with
    | None -> page
    | Some letter -> match List.find_opt (fun (s, _) -> String.compare letter s == 0) first_letters with
      | None -> page
      | Some (_, idx) -> idx / page_size
  in
  let page = min (page_count - 1) @@ max 0 page in
  let first_idx = page * page_size in
  seek_in ic (pos_in ic + (first_idx * 4));
  let iper_offset = input_binary_int ic in
  seek_in ic iper_offset;
  let page_size = if page_size * (page + 1) > person_count
    then person_count - (page_size * page)
    else page_size
  in
  let ipers = Array.make page_size Gwdb.dummy_iper in
  for i = 0 to page_size - 1 do
    Array.unsafe_set ipers i @@ input_value ic
  done ;
  close_in ic ;
  page_count, first_letters, ipers, page

let restricted_wizard fn self conf base =
  if conf.wizard then fn self conf base
  else self.RequestHandler.incorrect_request self conf base

let restricted_friend fn self conf base =
  if conf.wizard || conf.friend then fn self conf base
  else self.RequestHandler.incorrect_request self conf base

let with_person self conf base fn =
  match Util.find_person_in_env conf base "" with
  | Some p -> fn p
  | None -> self.RequestHandler.very_unknown self conf base

let birth_death_aux_fam conf base fn bool =
  List.map
    (fun (ifam, fam, d, c) ->
       let family = Data.get_n_mk_family conf base ifam fam in
       let date = Data.mk_date (Dgreg (d, c) ) in
       Tpat (function
           | "family" -> family
           | "date" -> date
           | _ -> raise Not_found) )
    (fst @@ BirthDeath.select_family conf base fn bool)

let birth_death_aux conf base fn bool =
  (* FIXME: do not always load ? *)
  let () = load_persons_array base in
  let list =
    List.map
      (fun (p, d, c) ->
         let person = Data.get_n_mk_person conf base (Gwdb.get_iper p) in
         let date = Data.mk_date (Dgreg (d, c) ) in
         Tpat (function
             | "person" -> person
             | "date" -> date
             | _ -> raise Not_found) )
      (fst @@ BirthDeath.select conf base fn bool)
  in
  (* FIXME: do not always load ? *)
  let () = clear_persons_array base in
  list

let handler =
  let open RequestHandler in
  { defaultHandler with

    _no_mode = defaultHandler._no_mode

  ; b = restricted_friend begin fun _self conf base ->
      let data = birth_death_aux conf base (fun p -> Adef.od_of_cdate (get_birth p)) false in
      let models = ("data", Tlist data) :: Data.default_env conf base in
      Interp.render ~conf ~file:"b" ~models
    end

  ; chg_chn = restricted_wizard begin fun self conf base ->
        match Util.p_getenv conf.env "ip" with
        | Some ip ->
          let ip = Gwdb.iper_of_string ip in
          let digest = ChangeChildren.digest_children base (Ezgw.Person.children base @@ Gwdb.poi base ip) in
          let models =
            ("digest", Tstr digest)
            :: ("ind", Data.get_n_mk_person conf base ip)
            :: Data.default_env conf base
          in
          Interp.render ~conf ~file:"chg_chn" ~models
        | _ -> self.incorrect_request self conf base
    end

  ; chg_chn_ok = restricted_wizard begin fun self conf base ->
      match Util.p_getenv conf.env "ip" with
      | None -> self.incorrect_request self conf base
      | Some i ->
        let conf = Update.update_conf conf in
        if Util.p_getenv conf.env "return" <> None then ChangeChildrenDisplay.print_update_child conf base
        else begin
          let models =
            try
              let ip = Gwdb.iper_of_string i in
              let p = Gwdb.poi base ip in
              let ipl = Ezgw.Person.children base p in
              ChangeChildren.check_digest conf (ChangeChildren.digest_children base ipl);
              let parent_surname = p_surname base p in
              let changed = ChangeChildren.change_children conf base parent_surname ipl in
              Util.commit_patches conf base;
              let changed =
                Def.U_Change_children_name
                  (Util.string_gen_person base (gen_person_of_person p), changed)
              in
              History.record conf base changed "cn";
              ("ind", Data.get_n_mk_person conf base ip) :: Data.default_env conf base
            with
            | ChangeChildren.FirstNameMissing _ip ->
              ("error", Tbool true) :: Data.default_env conf base
            | ChangeChildren.ChangeChildrenConflict (p, p') ->
              ChangeChildrenDisplay.print_conflict conf base (get_iper p) p'
          in
          Interp.render ~conf ~file:"chg_chn_ok" ~models
        end
    end

  ; d = begin fun self conf base -> with_person self conf base @@ fun p ->
      match Util.p_getenv conf.env "t", Util.p_getint conf.env "v" with
      | Some "A", Some v ->
        let models =
          ("ind", Data.unsafe_mk_person conf base p)
          :: ("max_level", Tint (min (Perso.limit_desc conf) v))
          :: ("num_aboville", Tbool (Util.p_getenv conf.env "num" = Some "on"))
          :: Data.default_env conf base
        in
        Interp.render ~conf ~file:"d_aboville" ~models
      | _ -> RequestHandler.defaultHandler.d self conf base
    end

  ; del_ind = restricted_wizard begin fun self conf base ->
      match Util.p_getenv conf.env "i" with
      | Some i ->
        let models =
          ("ind", Data.get_n_mk_person conf base (Gwdb.iper_of_string i))
          :: Data.default_env conf base
        in
        Interp.render ~conf ~file:"del_ind" ~models
      | _ -> self.incorrect_request self conf base
    end

  ; del_ind_ok = restricted_wizard begin fun self conf base ->
      match Util.p_getenv conf.env "i" with
      | Some i ->
        let ip = Gwdb.iper_of_string i in
        let p = poi base ip in
        let fn = sou base (get_first_name p) in
        let sn = sou base (get_surname p) in
        let old_related = get_related p in
        let op = Util.string_gen_person base (gen_person_of_person p) in
        UpdateIndOk.update_relations_of_related base ip old_related;
        let warnings = ref [] in
        let warning w = warnings := w :: !warnings in
        let p = UpdateIndOk.effective_del base warning p in
        Gwdb.patch_person base ip p;
        if fn <> "?" && sn <> "?" then
          Util.patch_cache_info conf Util.cache_nb_base_persons
            (fun v -> let v = int_of_string v - 1 in string_of_int v);
        Notes.update_notes_links_db conf (NotesLinks.PgInd p.key_index) "";
        Util.commit_patches conf base;
        let changed = Def.U_Delete_person op in
        History.record conf base changed "dp";
        let models =
          ("warnings", Tlist (List.map (Data.mk_warning conf base) !warnings))
          :: Data.default_env conf base
        in
        Interp.render ~conf ~file:"del_ind_ok" ~models
      | _ -> self.incorrect_request self conf base
    end

  ; ll = begin fun _self conf base ->
      let get_longest p =
        if Util.authorized_age conf base p then
          match get_death p with
          | Death (_, cd) -> begin match Adef.date_of_cdate cd with
              | Dgreg (dd, _) -> begin match Adef.od_of_cdate (get_birth p) with
                  | Some (Dgreg (bd, _)) ->
                    Some (Def.Dgreg (Date.time_elapsed bd dd, Dgregorian))
                  | _ -> None
                end
              | _ -> None
            end
          | _ -> None
        else None
      in
      let data = fst (BirthDeath.select conf base get_longest false) in
      let data =
        List.map
          (fun (p, d, c) ->
             Tset [ Data.get_n_mk_person conf base (Gwdb.get_iper p)
                  ; Data.mk_date (Dgreg (d, c) ) ] )
          (data)
      in
      let models =
        ("data", Tlist data)
        :: Data.default_env conf base
      in
      Interp.render ~conf ~file:"ll" ~models
    end

  ; lm = restricted_friend begin fun _self conf base ->
      let get_date _ fam =
        let rel = get_relation fam in
        if rel = Married || rel = NoSexesCheckMarried then
          Adef.od_of_cdate (get_marriage fam)
        else None
      in
      let data = birth_death_aux_fam conf base get_date false in
      let models = ("data", Tlist data) :: Data.default_env conf base in
      Interp.render ~conf ~file:"lm" ~models
    end

  ; h = begin fun self conf base ->
      match Util.p_getenv conf.env "v" with
      | Some "search_list" ->
        Interp.render
          ~conf
          ~file:"search_list"
          ~models:(Data.default_env conf base)
      | Some f -> SrcfileDisplay.print conf base f
      | None -> self.incorrect_request self conf base
    end

  ; mod_data = restricted_wizard begin fun self conf base ->
      match Util.p_getenv conf.env "data" with
      | Some ("place" | "src" | "occu") | None ->
        defaultHandler.mod_data self conf base
      | _ ->
        self.incorrect_request self conf base
    end

  ; mrg = restricted_wizard begin fun self conf base ->
      with_person self conf base @@ fun p ->
      let this_key_index = get_iper p in
      let list =
        Gutil.find_same_name base p
        |> (fun list ->
            List.fold_right
              (fun p l ->
                 if get_iper p = this_key_index then l
                 else Data.get_n_mk_person conf base (get_iper p) :: l)
              list [])
      in
      let models =
        ("suggestions", Tlist list)
        :: ("ind", Data.get_n_mk_person conf base this_key_index)
        :: Data.default_env conf base
      in
      Interp.render ~conf ~file:"mrg" ~models
    end

  ; mrg_ind = restricted_wizard begin fun _self conf base ->
      try match Util.p_getenv conf.env "i" with
        | None -> raise Not_found
        | Some i ->
          let ip1 = Gwdb.iper_of_string i in
          let ip2 =
            match Util.p_getenv conf.env "i2" with
            | Some i2 -> Gwdb.iper_of_string i2
            | None -> match Util.p_getenv conf.env "select", Util.p_getenv conf.env "n" with
              | (Some "input" | None), Some n ->
                begin match Gutil.person_ht_find_all base n with
                  | [ip2] -> ip2
                  | _ -> raise Not_found
                end
              | Some x, (Some "" | None) -> Gwdb.iper_of_string x
              | _ -> raise Not_found
          in
          let p1 = Gwdb.poi base ip1 in
          let p2 = Gwdb.poi base ip2 in
          let env = Data.default_env conf base in
          let propose_merge_ind conf base branches p1 p2 =
            let branches =
              Tlist
                (List.map (fun (i1, i2) ->
                     Tset [ Tlazy (lazy (Data.get_n_mk_person conf base i1) )
                          ; Tlazy (lazy (Data.get_n_mk_person conf base i2) ) ] )
                    branches)
            in
            let env = Data.default_env conf base in
            let p1 = Data.unsafe_mk_person conf base p1 in
            let p2 = Data.unsafe_mk_person conf base p2 in
            let propose_merge_ind = Tbool true in
            let models =
              ( "data"
              , Tpat (function "propose_merge_ind" -> propose_merge_ind
                             | "branches" -> branches
                             | "p1" -> p1
                             | "p2" -> p2
                             | _ -> raise Not_found ) )
              :: env
            in
            Interp.render ~conf ~file:"mrg_ind" ~models
          in
          try
            let propose_merge_fam = Geneweb.MergeIndDisplay.propose_merge_fam in (* FIXME *)
            let (ok, wl) = Geneweb.MergeInd.merge conf base p1 p2 propose_merge_ind propose_merge_fam in
            if ok then
              let wl = List.map (Data.mk_warning conf base) wl in
              let models =
                ("data", Tpat (function "warnings" -> Tlist wl
                                      | "merged_ind" -> Data.unsafe_mk_person conf base p1
                                      | _ -> raise Not_found ) )
                :: env
              in
              Interp.render ~conf ~file:"mrg_ind" ~models
          with
          | Geneweb.MergeInd.Same_person ->
            let models = ("error", Tpat (function "same_person" -> Tbool true | _ -> raise Not_found) ) :: env in
            Interp.render ~conf ~file:"mrg_ind" ~models
          | Geneweb.MergeInd.Different_sexes ->
            let models = ( "error", Tpat (function "different_sexes" -> Tbool true | _ -> raise Not_found) ) :: env  in
            Interp.render ~conf ~file:"mrg_ind" ~models
          | Geneweb.MergeInd.Error_loop p ->
            let ind = Data.unsafe_mk_person conf base p in
            let models = ( "error", Tpat (function "own_ancestor" -> ind | _ -> raise Not_found) ) :: env  in
            Interp.render ~conf ~file:"mrg_ind" ~models
        with Not_found ->
          Interp.render ~conf ~file:"mrg_ind" ~models:( ("error", Tbool true) :: Data.default_env conf base )
      end

  ; notes = begin fun self conf base ->
      let fnotes =
        match Util.p_getenv conf.env "f" with
        | Some f when NotesLinks.check_file_name f <> None -> f (* Usefulness? *)
        | _ -> ""
      in
      match Util.p_getint conf.env "v" with
      | None when Util.p_getenv conf.env "ref" <> Some "on" ->
        let (nenv, note) = Notes.read_notes base fnotes in
        let title =
          try Util.safe_html (List.assoc "TITLE" nenv)
          with Not_found -> ""
        in
        let file_path = Notes.file_path conf base in (* FIXME? fnotes? *)
        let note = Util.string_with_macros conf [] note in
        let edit_opt = Some (conf.wizard, "NOTES", fnotes) in
        let note =
          let wi =
            { Wiki.wi_mode = "NOTES"
            ; Wiki.wi_file_path = file_path
            ; Wiki.wi_cancel_links = conf.cancel_links
            ; Wiki.wi_person_exists = Util.person_exists conf base
            ; Wiki.wi_always_show_link = conf.wizard || conf.friend
            }
          in
          Wiki.html_with_summary_of_tlsw conf wi edit_opt note
        in
        let note = Util.safe_html note in
        let models =
          ("note", Tstr note)
          :: ("title", Tstr title)
          :: ("fnotes", Tstr fnotes)
          :: Data.default_env conf base in
        Interp.render ~conf ~file:"notes_full" ~models

      | _ -> defaultHandler.notes self conf base
    end

  ; oa = restricted_friend @@ begin fun _self conf base ->
      let limit = Opt.default 0 (Util.p_getint conf.env "lim") in
      let get_oldest_alive p = match get_death p with
        | NotDead -> Adef.od_of_cdate (get_birth p)
        | DontKnowIfDead when limit > 0 ->
          begin match Adef.od_of_cdate (get_birth p) with
            | Some (Dgreg (d, _)) as x when conf.today.year - d.year <= limit -> x
            | _ -> None
          end
        | _ -> None
      in
      let data = birth_death_aux conf base get_oldest_alive true in
      let models = ("data", Tlist data) :: Data.default_env conf base in
      Interp.render ~conf ~file:"oa" ~models
    end

  ; oe = begin restricted_friend @@ fun _self conf base ->
      let get_date = fun _ fam ->
        if get_relation fam = Engaged then
          let husb = Util.pget conf base (get_father fam) in
          let wife = Util.pget conf base (get_mother fam) in
          match get_death husb, get_death wife with
          | (NotDead | DontKnowIfDead), (NotDead | DontKnowIfDead) ->
            Adef.od_of_cdate (get_marriage fam)
          | _ -> None
        else None
      in
      let data = birth_death_aux_fam conf base get_date true in
      let models = ("data", Tlist data) :: Data.default_env conf base in
      Interp.render ~conf ~file:"oe" ~models
    end

  ; pop_pyr = begin fun _self conf base ->
      let interval = max 1 (Opt.default 5 @@ Util.p_getint conf.env "int") in
      let limit = Opt.default 0 @@ Util.p_getint conf.env "lim" in
      let at_date =
        Opt.map_default conf.today
          (fun i -> {year = i; month = 31; day = 12; prec = Sure; delta = 0})
          (Util.p_getint conf.env "y")
      in
      let nb_intervals = 150 / interval in
      let men, wom = BirthDeath.make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base in
      let models =
        ("nb_intervals", Tint nb_intervals)
        :: ("interval", Tint interval)
        :: ("limit", Tint limit)
        :: ("year", Tint at_date.year)
        :: ("men", Tarray (Array.map box_int men))
        :: ("wom", Tarray (Array.map box_int wom))
        :: Data.default_env conf base
      in
      Interp.render ~conf ~file:"pop_pyr" ~models
    end

  ; fallback = begin fun mode -> fun self conf base ->
      match mode with

      | "API_PRINT_EXPORT" ->
        Api_conf.set_mode_api () ;
        Api_app.print_export conf base

      | "API_PRINT_EXPORT_SEARCH" ->
        Api_conf.set_mode_api () ;
        Api_app.print_export_search conf base

      | "API_PRINT_SYNCHRO" ->
        Api_conf.set_mode_api () ;
        Api_app.print_synchro_patch_mobile conf base

      | "SANDBOX" ->
        restricted_wizard
          (fun _self conf base ->
             Interp.render_jingoo
               ~file:"sandbox.jingoo" ~models:(Data.sandbox conf base) )
          self conf base

      | "TIMELINE" ->
        with_person self conf base (fun p ->
            let root = Data.unsafe_mk_person conf base p in
            let models = ("root", root) :: Data.default_env conf base in
            Interp.render ~conf ~file:"timeline" ~models
          )

      | "LIST_IND" ->
        begin
          let num = (Opt.default 1 @@ Util.p_getint conf.env "pg") - 1 in
          let size = Opt.default 2000 @@ Util.p_getint conf.env "sz" in
          let letter = Util.p_getenv conf.env "letter" in
          if not (is_cache_iper_inorder_uptodate conf base) then build_cache_iper_inorder conf base ;
          let page_count, first_letters, ipers, num = read_cache_iper_inorder conf num size letter in
          let persons = Array.map (fun i -> Data.unsafe_mk_person conf base @@ Gwdb.poi base i) ipers in
          let anchorAtIndex =
            let fst_idx = size * num in
            let list = List.map (fun (c, i) -> (i - fst_idx, c)) first_letters  in
            Jg_types.func_arg1_no_kw @@ function
            | Tint i ->
              begin match List.assoc_opt i list with Some s -> Tstr s | None -> Tnull end
            | x -> Jg_types.func_failure [x]
          in
          let letter_list = List.map (fun (l, _) -> Tstr l) first_letters in
          let models = ("letter_list", Tlist letter_list)
                       :: ("anchorAtIndex", anchorAtIndex)
                       :: ("persons", Tarray persons)
                       :: ("page_num", Tint (num + 1))
                       :: ("page_count", Tint page_count)
                       :: Data.default_env conf base
          in
          Interp.render ~conf ~file:"list_ind" ~models
        end

      | _ -> self.RequestHandler.incorrect_request self conf base
    end

  }
