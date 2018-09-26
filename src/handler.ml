(* TODO check "unsafe" stuff *)

open Geneweb
open Config
open Def
open Gwdb
open Util

open Jg_types

let birth_death_aux_fam conf base fn bool =
  List.map
    (fun (ifam, fam, d, c) ->
       let family = Data.get_n_mk_family conf base ifam fam in
       let date = Data.mk_date conf (Dgreg (d, c) ) in
       Tpat (function
           | "family" -> family
           | "date" -> date
           | _ -> raise Not_found) )
    (fst @@ BirthDeath.select_family conf base fn bool)

let print_oe conf base =
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
  Interp.render ~file:"oldest_engagement" ~models

let print_lm conf base =
  let get_date _ fam =
    let rel = get_relation fam in
    if rel = Married || rel = NoSexesCheckMarried then
      Adef.od_of_cdate (get_marriage fam)
    else None
  in
  let data = birth_death_aux_fam conf base get_date false in
  let models = ("data", Tlist data) :: Data.default_env conf base in
  Interp.render ~file:"latest_marriage" ~models

let birth_death_aux conf base fn bool =
  (* FIXME: do not always load ? *)
  let () = load_persons_array base in
  let list =
    List.map
      (fun (p, d, c) ->
         let person = Data.get_n_mk_person conf base (Gwdb.get_key_index p) in
         let date = Data.mk_date conf (Dgreg (d, c) ) in
         Tpat (function
             | "person" -> person
             | "date" -> date
             | _ -> raise Not_found) )
      (fst @@ BirthDeath.select conf base fn bool)
  in
  (* FIXME: do not always load ? *)
  let () = clear_persons_array base in
  list

let print_birth conf base =
  let data = birth_death_aux conf base (fun p -> Adef.od_of_cdate (get_birth p)) false in
  let models = ("data", Tlist data) :: Data.default_env conf base in
  Interp.render ~file:"latest_birth" ~models

let print_death conf base =
  let get_death p =
    match get_death p with
    | Death (_, cd) -> Some (Adef.date_of_cdate cd)
    | _ -> None
  in
  let data = birth_death_aux conf base get_death false in
  let models = ("data", Tlist data) :: Data.default_env conf base in
  Interp.render ~file:"latest_death" ~models

let print_oa conf base =
  let limit =
    match p_getint conf.env "lim" with
      Some x -> x
    | _ -> 0
  in
  let get_oldest_alive p =
    match get_death p with
    | NotDead -> Adef.od_of_cdate (get_birth p)
    | DontKnowIfDead when limit > 0 ->
        begin match Adef.od_of_cdate (get_birth p) with
          Some (Dgreg (d, _)) as x when conf.today.year - d.year <= limit -> x
        | _ -> None
        end
    | _ -> None
  in
  let data = birth_death_aux conf base get_oldest_alive true in
  let models =
    ("data", Tlist data)
    :: Data.default_env conf base
  in
  Interp.render ~file:"oldest_alive" ~models

let print_ll conf base =
  let get_longest p =
    if Util.authorized_age conf base p then
      match Adef.od_of_cdate (get_birth p), get_death p with
        Some (Dgreg (bd, _)), Death (_, cd) ->
          begin match Adef.date_of_cdate cd with
            Dgreg (dd, _) ->
              Some (Dgreg (CheckItem.time_elapsed bd dd, Dgregorian))
          | _ -> None
          end
      | _ -> None
    else None
  in
  let data = fst (BirthDeath.select conf base get_longest false) in
  let data =
    List.map
      (fun (p, d, c) ->
         Tset [ Data.get_n_mk_person conf base (Gwdb.get_key_index p)
              ; Data.mk_date conf (Dgreg (d, c) ) ] )
      (data)
  in
  let models =
    ("data", Tlist data)
    :: Data.default_env conf base
  in
  Interp.render ~file:"longest_lived" ~models

let print_pop_pyr conf base =
  let interval =
    match p_getint conf.env "int" with
      Some i -> max 1 i
    | None -> 5
  in
  let limit =
    match p_getint conf.env "lim" with
      Some x -> x
    | _ -> 0
  in
  let at_date =
    match p_getint conf.env "y" with
      Some i -> {year = i; month = 31; day = 12; prec = Sure; delta = 0}
    | None -> conf.today
  in
  let nb_intervals = 150 / interval in
  let men, wom = BirthDeath.make_population_pyramid ~nb_intervals ~interval ~limit ~at_date conf base in
  let models =
    ("nb_intervals", Tint nb_intervals)
    :: ("interval", Tint interval)
    :: ("limit", Tint limit)
    :: ("year", Tint at_date.year)
    :: ("men", Tarray (Array.map Jg_runtime.box_int men))
    :: ("wom", Tarray (Array.map Jg_runtime.box_int wom))
    :: Data.default_env conf base
  in
  Interp.render ~file:"pop_pyr" ~models

let print_chg_chn conf base ip =
  let models =
    ("ind", Data.get_n_mk_person conf base ip)
    :: Data.default_env conf base
  in
  Interp.render ~file:"chg_chn" ~models

let print_chg_chn_ok conf base ip =
  try
    if p_getenv conf.env "return" <> None then print_chg_chn conf base ip
    else
      begin
        let p = Gwdb.poi base ip in
        let ipl = Ezgw.Person.children base p in
        let parent_surname = p_surname base p in
        ChangeChildren.check_digest conf (ChangeChildren.digest_children base ipl);
        let changed =
          try ChangeChildren.change_children conf base parent_surname ipl
          with ChangeChildren.FirstNameMissing _ip -> failwith "TODO"
        in
        Util.commit_patches conf base;
        let changed =
          U_Change_children_name
            (Util.string_gen_person base (gen_person_of_person p), changed)
        in
        History.record conf base changed "cn";
        let models =
          ("ind", Data.get_n_mk_person conf base ip)
          :: Data.default_env conf base
        in
        Interp.render ~file:"chg_chn_ok" ~models
      end
  with Update.ModErr -> ()      (* FIXME? *)

let print_d self conf base ip =
  match p_getenv conf.env "t", p_getint conf.env "v" with
  | Some "A", Some v ->
    let models =
      ("ind", Data.get_n_mk_person conf base ip)
      :: ("max_level", Tint (min (Perso.limit_desc conf) v))
      :: ("num_aboville", Tbool (p_getenv conf.env "num" = Some "on"))
      :: Data.default_env conf base
    in
    Interp.render ~file:"descend.print_aboville" ~models
  | _ -> RequestHandler.defaultHandler.d self conf base

let print_del_ind conf base i =
  let models =
    ("data", Data.get_n_mk_person conf base i)
    :: Data.default_env conf base
  in
  Interp.render ~file:"del_ind" ~models

let print_del_ind_ok conf base =
  let models = Data.default_env conf base in
  Interp.render ~file:"del_ind" ~models

let print_mrg conf base p =
  let this_key_index = get_key_index p in
  let list =
    Gutil.find_same_name base p
    |> (fun list ->
        List.fold_right
          (fun p l ->
             if get_key_index p = this_key_index then l
             else Data.get_n_mk_person conf base (get_key_index p) :: l)
          list [])
  in
  let models =
    ("suggestions", Tlist list)
    :: ("ind", Data.get_n_mk_person conf base this_key_index)
    :: Data.default_env conf base
  in
  Interp.render ~file:"mrg" ~models

let alln_select conf base is_surnames ini need_whole_list =
  (* Do not alway load *)
  let () = load_strings_array base in
  let (list, _, _) = Alln.select_names conf base is_surnames ini need_whole_list in
  let () = clear_strings_array base in
  list

let print_frequency conf base is_surnames =
  let list =
    List.map (fun (_k, s, c) ->
        (* let key = Tstr k in *)
        let value = Tstr s in
        let count = Tint c in
        (* let alphab_string = Tstr (Alln.alphab_string base is_surnames s) in *)
        Tpat (function
            (* | "key" -> key *)
            | "value" -> value
            | "count" -> count
            (* | "alphab_string" -> alphab_string *)
            | _ -> raise Not_found) )
      (alln_select conf base is_surnames "" true)
  in
  let models =
    ("list", Tlist list)
    :: Data.default_env conf base
  in
  Interp.render ~file:"frequency" ~models

let print_short conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    | Some k -> k
    | _ -> ""
  in
  let list =
    List.map (fun (_k, s, _c) ->
        let value = Tstr s in
        let first_letter = Tstr (String.make 1 (String.get s 0) ) in
        Tpat (function
            | "__str__" -> value
            | "first_letter" -> first_letter
            | _ -> raise Not_found) )
      (alln_select conf base is_surnames ini true)
  in
  let models =
    ("list", Tlist list)
    :: Data.default_env conf base
  in
  Interp.render ~file:"names_alphabetic_short" ~models

let print_alln conf base surname =
  match p_getenv conf.env "tri" with
  | Some "F" -> print_frequency conf base surname
  | Some "S" -> print_short conf base surname
  | _ -> failwith __LOC__
  (* | _ -> print_alphabetic conf base surname *)

let print_mrg_ind conf base ip1 ip2 =
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
      :: ( "string_of_burial", Tfun (fun _ _ -> Tnull))
      :: env
    in
    Interp.render ~file:"mrg_ind" ~models
  in
  try
    let (ok, wl) = Geneweb.MergeInd.merge conf base p1 p2 propose_merge_ind in
    if ok then
      let wl = List.map (Data.mk_warning conf base) wl in
      let models =
        ("data", Tpat (function "warnings" -> Tlist wl
                              | "merged_ind" -> Data.unsafe_mk_person conf base p1
                              | _ -> raise Not_found ) )
        :: env
      in
      Interp.render ~file:"mrg_ind" ~models
  with
  | Geneweb.MergeInd.Same_person ->
    let models = ("error", Tpat (function "same_person" -> Tbool true | _ -> raise Not_found) ) :: env in
    Interp.render ~file:"mrg_ind" ~models
  | Geneweb.MergeInd.Different_sexes ->
    let models = ( "error", Tpat (function "different_sexes" -> Tbool true | _ -> raise Not_found) ) :: env  in
    Interp.render ~file:"mrg_ind" ~models
  | Geneweb.MergeInd.Error_loop p ->
    let models = ( "error", Data.unsafe_mk_person conf base p ) :: env  in
    Interp.render ~file:"mrg_ind" ~models

let _no_mode self conf base =
  print_endline __LOC__ ;
  match find_person_in_env conf base "" with
  | Some p ->
    let models =
      ("ind", Data.get_n_mk_person conf base (Gwdb.get_key_index p) )
      :: Data.default_env conf base
    in
    let file =
      let etatcivil = ref 1 in
      let parent = ref 1 in
      let union = ref 1 in
      let freresoeur = ref 1 in
      let relations = ref 1 in
      let famille = ref 0 in
      let notes = ref 1 in
      let sources = ref 1 in
      let arbre = ref 2 in
      (* let timeline = ref "0" in *)
      let () =
        print_endline __LOC__ ;
        match List.assoc_opt "p_mod" conf.env with
        | Some s when String.length s = 18 ->
          print_endline __LOC__ ;
          let read i = (Char.code @@ String.unsafe_get s i) - 48 in
          assert (String.unsafe_get s 0 = 'i') ;
          etatcivil := read 1 ;
          assert (String.unsafe_get s 2 = 'p') ;
          parent := read 3 ;
          assert (String.unsafe_get s 4 = 'u') ;
          union := read 5 ;
          assert (String.unsafe_get s 6 = 'f') ;
          freresoeur := read 7 ;
          assert (String.unsafe_get s 8 = 'r') ;
          relations := read 9 ;
          assert (String.unsafe_get s 10 = 'g') ;
          famille := read 11 ;
          assert (String.unsafe_get s 12 = 'n') ;
          notes := read 13 ;
          assert (String.unsafe_get s 14 = 's') ;
          sources := read 15 ;
          assert (String.unsafe_get s 16 = 'a') ;
          arbre := read 17 ;
        | Some _ -> print_endline __LOC__ ; assert false
        | None ->
          let n =
            try int_of_string @@ List.assoc "module_perso_tplnb" conf.base_env
            with _ -> 0
          in
          print_endline __LOC__ ;
          for i = 0 to n - 1 do
            try match List.assoc ("module_perso_" ^ string_of_int i) conf.base_env with
              | "arbre_1" -> arbre := 1
              | "arbre_2" -> arbre := 2
              | "arbre_3" -> arbre := 3
              | "arbre_4" -> arbre := 4
              | "etatcivil_1" -> etatcivil := 1
              | "famille_1" -> famille := 1
              | "freresoeur_1" -> freresoeur := 1
              | "freresoeur_3" -> freresoeur := 3
              | "notes_1" -> notes := 1
              | "parent_1" -> parent := 1
              | "parent_2" -> parent := 2
              | "parent_3" -> parent := 3
              | "parent_4" -> parent := 4
              | "relations_1" -> relations := 1
              | "sources_1" -> sources := 1
              (* | "timeline_1" -> timeline := 1 *)
              | "union_1" -> union := 1
              | "union_2" -> union := 2
              | "union_3" -> union := 3
              | "union_4" -> union := 4
              | s -> failwith s
            with _ ->
              List.iter print_endline @@ List.map fst conf.base_env ; assert false
          done
      in
      Printf.sprintf
        "perso_templates/perso_%d%d%d%d%d%d%d%d%d"
        !etatcivil
        !parent
        !union
        !freresoeur
        !relations
        !famille
        !notes
        !sources
        !arbre
    in
    Interp.render ~file ~models
  | _ -> self.RequestHandler.very_unknown self conf base

let restricted_wizard fn self conf base =
  if conf.wizard then fn self conf base
  else self.RequestHandler.incorrect_request self conf base

let restricted_friend fn self conf base =
  if conf.wizard || conf.friend then fn self conf base
  else self.RequestHandler.incorrect_request self conf base

let handler =
  let open RequestHandler in
  { defaultHandler
    with

      _no_mode

    ; b = begin restricted_friend @@ fun _self conf base ->
        print_birth conf base
      end

    ; chg_chn = begin restricted_wizard @@ fun self conf base ->
        match p_getint conf.env "ip" with
        | Some i -> print_chg_chn conf base (Adef.iper_of_int i)
        | _ -> self.incorrect_request self conf base
      end

    ; chg_chn_ok = begin restricted_wizard @@ fun self conf base ->
        match p_getint conf.env "ip" with
        | Some i -> print_chg_chn_ok conf base (Adef.iper_of_int i)
        | _ -> self.incorrect_request self conf base
      end

    ; d = begin fun self conf base ->
        match find_person_in_env conf base "" with
        | Some p -> print_d self conf base (Gwdb.get_key_index p)
        | _ -> self.very_unknown self conf base
      end

    ; del_ind = begin restricted_wizard @@ fun self conf base ->
        match p_getint conf.env "i" with
        | Some i -> print_del_ind conf base (Adef.iper_of_int i)
        | _ -> self.incorrect_request self conf base
      end

    ; del_ind_ok = begin restricted_wizard @@ fun self conf base ->
        match p_getint conf.env "i" with
        | Some i ->
          let ip = Adef.iper_of_int i in
          let p = poi base ip in
          let fn = sou base (get_first_name p) in
          let sn = sou base (get_surname p) in
          let occ = get_occ p in
          let old_related = get_related p in
          let op = Util.string_gen_person base (gen_person_of_person p) in
          UpdateIndOk.update_relations_of_related base ip old_related;
          let warning _ = () in     (* TODO!!! *)
          let p = UpdateIndOk.effective_del base warning p in
          Gwdb.patch_person base ip p;
          if fn <> "?" && sn <> "?" then
            Util.patch_cache_info conf Util.cache_nb_base_persons
              (fun v -> let v = int_of_string v - 1 in string_of_int v);
          Gwdb.delete_key base fn sn occ;
          Notes.update_notes_links_db conf (NotesLinks.PgInd p.key_index) "";
          Util.commit_patches conf base;
          let changed = U_Delete_person op in
          History.record conf base changed "dp";
          print_del_ind_ok conf base
        | _ -> self.incorrect_request self conf base
      end

    ; lb = begin fun self conf base -> self.b self conf base end

    ; ll = begin fun _self conf base ->
        print_ll conf base
      end

    ; mrg = begin restricted_wizard @@ fun self conf base ->
        match find_person_in_env conf base "" with
        | Some p -> print_mrg conf base p
        | _ -> self.very_unknown self conf base
      end

    ; mrg_ind = begin restricted_wizard @@ fun _self conf base ->
        try match p_getint conf.env "i" with
          | None -> raise Not_found
          | Some i ->
            let ip1 = Adef.iper_of_int i in
            let ip2 =
              match p_getint conf.env "i2" with
              | Some i2 -> Adef.iper_of_int i2
              | None -> match p_getenv conf.env "select", p_getenv conf.env "n" with
                | (Some "input" | None), Some n ->
                  begin match Gutil.person_ht_find_all base n with
                    | [ip2] -> ip2
                    | _ -> raise Not_found
                  end
                | Some x, (Some "" | None) -> Adef.iper_of_int (int_of_string x)
                | _ -> raise Not_found
            in
            print_mrg_ind conf base ip1 ip2
        with Not_found ->
          Interp.render ~file:"mrg_ind" ~models:( ("error", Tbool true) :: Data.default_env conf base )
      end

    ; n = begin fun _self conf base ->
        match p_getenv conf.env "v" with
        | Some v -> Some.surname_print conf base Some.surname_not_found v
        | _ ->
          let () = load_persons_array base in
          print_alln conf base true
      end

    ; oa = begin restricted_friend @@ fun _self conf base ->
        print_oa conf base
      end

    ; oe = begin restricted_friend @@ fun _self conf base ->
        print_oe conf base
      end

    (* ; p = begin fun _self conf base ->
     *     match p_getenv conf.env "v" with
     *     | Some v -> Some.first_name_print conf base v
     *     | None -> print_alln conf base false
     *   end *)

    ; pop_pyr = begin restricted_friend @@ fun _self conf base ->
        print_pop_pyr conf base
      end

  }

