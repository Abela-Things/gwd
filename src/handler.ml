open Geneweb
open Gwdb
open Config
open Jingoo
open Jg_types

let _bench name fn =
  let p1 = Sys.time () in
  let t1 = Unix.gettimeofday () in
  let res = fn () in
  let t2 = Unix.gettimeofday () in
  let p2 = Sys.time () in
  Printf.printf "[%s]: %f seconds (~%f seconds of CPU time).\n" name (t2 -. t1) (p2 -. p1);
  res

let lower_fst =
  let b = Buffer.create 6 in
  fun s ->
    Buffer.reset b ;
    let _, i0, _ = Utf8.C.unaccent false s 0 (String.length s) in
    let u = Utf8.C.cp s i0 in
    begin match Uucp.Case.Map.to_lower u with
      | `Self -> Uutf.Buffer.add_utf_8 b u
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
    end ;
    Buffer.contents b

let homonyms_file conf =
  Filename.concat (Util.base_path [] (conf.bname ^ ".gwb")) "cache_homonyms"

let homonyms_magic = 0xABE1A

let check_homonyms_magic conf =
  let ic = Secure.open_in_bin (homonyms_file conf) in
  let res = (input_binary_int ic) == homonyms_magic in
  close_in ic;
  res

let is_cache_homonyms_uptodate conf base =
  let cache_path = homonyms_file conf in
  try
    let cache_stat = Unix.stat (cache_path) in
    let cache_timeof_modif = cache_stat.Unix.st_mtime in
    let base_timeof_modif = Gwdb.date_of_last_change base in
    (base_timeof_modif < cache_timeof_modif) && check_homonyms_magic conf
  with
    Unix.Unix_error _ -> false

let read_cache_homonyms conf base page_num size =
  let cache_filename = homonyms_file conf in
  let ic = Secure.open_in_bin cache_filename in
  let _ = input_binary_int ic in (* ignoring magic number *)
  let h_count = input_binary_int ic in
  let page_count = h_count / size + if h_count mod size == 0 then 0 else 1 in
  let page_num = min (page_count - 1) @@ max 0 page_num in
  let start_idx = page_num * size in
  let size = if (start_idx + size > h_count) then (h_count - start_idx) else size in
  let rec get_first_homonym_idx idx =
    let _ = input_binary_int ic in
    let len = input_binary_int ic in
    if len + idx <= start_idx then get_first_homonym_idx (len + idx)
    else idx
  in
  let curr_idx = get_first_homonym_idx 0 in
  seek_in ic (pos_in ic - 8);
  let hidden_start_count = start_idx - curr_idx in
  let cache_homonyms =
    let rec get_curr_homonym len = match len with
      | 0 -> []
      | _ ->
        let iper : iper = input_value ic in
        (Data.unsafe_mk_person conf base @@ Gwdb.poi base iper) :: get_curr_homonym (len - 1)
    in
    let rec get_homonyms page_idx =
      if page_idx - hidden_start_count >= size then
        []
      else
        begin
        let offset = input_binary_int ic in
        let len = input_binary_int ic in
        let off_position = pos_in ic in
        seek_in ic offset;
        let homonym = get_curr_homonym len in
        seek_in ic off_position;
        Tlist homonym :: (get_homonyms (page_idx + len))
        end
    in
    get_homonyms 0
  in
  close_in ic;
  Printf.printf "homonyms cache red!\n%!";
  Printf.printf "homonyms groups : %d, page_num size : %d \n%!" (List.length cache_homonyms) size;
  cache_homonyms, hidden_start_count, h_count, page_count, page_num

type person_hash_table = ((string * string), person list) Hashtbl.t

let build_cache_homonyms conf base =
  Printf.printf "building cache_homonyms...\n%!";
  let (person_hash : person_hash_table) = Hashtbl.create (Gwdb.nb_of_persons base) in
  Gwdb.load_persons_array base ;
  Gwdb.load_strings_array base ;
  Gwdb.Collection.fold (fun _ p ->
    (* FIXME: stop checking is_empty_name when possible *)
    if not (Util.is_empty_name p) then
      let first_name = Ezgw.Person.first_name base p in
      let surname = Ezgw.Person.surname base p in
      match Hashtbl.find_opt person_hash (first_name, surname) with
        None ->
          Hashtbl.add person_hash (first_name, surname) [p]
      | Some pers ->
          Hashtbl.replace person_hash (first_name, surname) (p :: pers)
  ) () (Gwdb.persons base);
  let compare_homonyms h1 h2 =
    (* FIXME! replace this by the new comparison function when merging with Geneanet's version.*)
    let compare_names p1 p2 =
      match Gutil.alphabetic_utf_8
          (Util.name_key base @@ Ezgw.Person.surname base p1)
          (Util.name_key base @@ Ezgw.Person.surname base p2)
      with
      | 0 -> Gutil.alphabetic_utf_8 (Ezgw.Person.surname base p1) (Ezgw.Person.surname base p2)
      | x -> x
    in compare_names (List.hd h1) (List.hd h2)
  in
  let sorted_homonyms = List.sort compare_homonyms
    (Hashtbl.fold (fun _ sames homonyms ->
      if List.length sames > 1 then 
          sames :: homonyms
      else homonyms)
    person_hash [])
  in
  let cache_filename = homonyms_file conf in
  let oc = Secure.open_out_bin cache_filename in
  output_binary_int oc homonyms_magic; (* TODO only write magic number if building happened correctly *)
  output_binary_int oc 0 ; (* empty space to write total_count later *)
  let idx_offset = pos_out oc in
  List.iter (fun _ ->
      (* FIXME This part could be ignored if we knew the
      number of homonyms and the number of groups. *)
      output_binary_int oc 0;
      output_binary_int oc 0;
    ) sorted_homonyms;
  let rec output_homonyms h_list idx_offset =
    match h_list with
    | [] -> 0
    | hd :: tl ->
        let curr_count = List.length hd in
        let h_offset = pos_out oc in (*save current offset *)
        seek_out oc idx_offset;
        output_binary_int oc h_offset; (* write offset *)
        output_binary_int oc curr_count; (* write size of the current homonym group *)
        let idx_offset = pos_out oc in
        seek_out oc h_offset;
        List.iter (fun p ->
          Printf.printf "getting iper%!";
          Printf.printf "of %s %s\n%!" (Ezgw.Person.first_name base p) (Ezgw.Person.surname base p) ;
          output_value oc (Gwdb.get_iper p)) hd; (* write homonym group *)
        curr_count + output_homonyms tl idx_offset
  in
  let total_count = output_homonyms sorted_homonyms idx_offset in
  Printf.printf "Writing cache_homonym size (%d)...\n%!" total_count;
  seek_out oc 4;
  output_binary_int oc total_count;
  Gwdb.clear_persons_array base ;
  Gwdb.clear_strings_array base ;
  Printf.printf "cache_homonyms built!\n%!";
  close_out oc

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
      type t = (string * istr * int * iper)
      let compare (s1, f1, o1, _) (s2, f2, o2, _) =
        match Utf8.compare s1 s2 with
        | 0 -> begin
            match Utf8.compare (Gwdb.sou base f1) (Gwdb.sou base f2) with
            | 0 -> compare o1 o2
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
      else PerSet.add (Util.name_key base (sou base @@ get_surname p)
                      , get_first_name p
                      , get_occ p
                      , get_iper p) set
    end PerSet.empty (Gwdb.persons base)
  in
  Gwdb.clear_persons_array base ;
  Gwdb.clear_strings_array base ;
  let cache_filename = list_ind_file conf in
  let cnt = PerSet.cardinal set in
  let _, letters =
    PerSet.fold begin fun (x, _, _, _) (idx, acc) ->
      let c = lower_fst x in
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
  PerSet.fold begin fun (_, _, _, i) (m1, m2) ->
    seek_out oc m1 ;
    output_binary_int oc m2 ;
    let m1 = pos_out oc in
    seek_out oc m2 ;
    output_value oc i ;
    let m2 = pos_out oc in
    (m1, m2)
  end set (m1, m2) ;
  close_out oc

let read_cache_iper_inorder conf page page_size =
  let cache_filename = list_ind_file conf in
  let ic = Secure.open_in_bin cache_filename in
  let person_count = input_binary_int ic in
  let page_count = person_count / page_size + if person_count mod page_size == 0 then 0 else 1 in
  let first_letters : (string * int) list = input_value ic in
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

  ; as_ok = begin fun _self conf base ->
      let max_answers = try int_of_string @@ List.assoc "max" conf.env with _ -> 100 in
      let (list, _len) = AdvSearchOk.advanced_search conf base max_int in
      let searching_fields =
        let s = AdvSearchOk.searching_fields conf base in
        if s = "" then ""
        else if String.get s (String.length s - 1) == ','
        then String.sub s 0 (String.length s - 1)
        else s
      in
      let models =
        ("results", Tlist (List.map (Data.unsafe_mk_person conf base) list))
        :: ("max_results", Tint max_answers)
        :: ("searching_fields", Tstr searching_fields)
        :: Data.default_env conf base
      in
      Interp.render ~conf ~file:"as_ok" ~models
    end

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
      | Some (("search_list"|"search_event") as file) ->
        Interp.render ~conf ~file ~models:(Data.default_env conf base)
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
          if not (is_cache_iper_inorder_uptodate conf base)
          then build_cache_iper_inorder conf base ;
          let page_count, letters, ipers, num =
            read_cache_iper_inorder conf num size
          in
          let persons =
            Array.map begin fun i ->
              Data.unsafe_mk_person conf base @@ Gwdb.poi base i
            end ipers
          in
          let anchorAtIndex =
            let fst_idx = size * num in
            let list = List.map (fun (c, i) -> (i - fst_idx, c)) letters  in
            Jg_types.func_arg1_no_kw @@ function
            | Tint i ->
              begin match List.assoc_opt i list with
                |  Some s -> Tstr s
                | None -> Tnull
              end
            | x -> Jg_types.func_failure [x]
          in
          let letters =
            List.map begin fun (c, i) ->
              Tset [ Tstr c ; Tint (i / size + 1) ]
            end letters
          in
          let models = ("letters", Tlist letters)
                       :: ("anchorAtIndex", anchorAtIndex)
                       :: ("persons", Tarray persons)
                       :: ("page_num", Tint (num + 1))
                       :: ("page_count", Tint page_count)
                       :: Data.default_env conf base
          in
          Interp.render ~conf ~file:"list_ind" ~models
        end

      | "HOMONYMS" ->
        begin
          let page = (Opt.default 0 @@ Util.p_getint conf.env "pg") - 1 in
          let size = Opt.default 10 @@ Util.p_getint conf.env "sz" in
          if not (is_cache_homonyms_uptodate conf base) then build_cache_homonyms conf base;
          let homonyms, hidden_count, h_count, page_count, page = read_cache_homonyms conf base page size in
          let models = ("hidden_count", Tint hidden_count)
                    :: ("homonym_count", Tint h_count)
                    :: ("homonyms", Tlist homonyms)
                    :: ("page_num", Tint (page + 1))
                    :: ("page_size", Tint size)
                    :: ("page_count", Tint page_count)
                    :: Data.default_env conf base
          in
          Interp.render ~conf ~file:"homonyms" ~models
        end

      | _ -> self.RequestHandler.incorrect_request self conf base
    end
  }

