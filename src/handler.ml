open Geneweb
open Gwdb
open Config
open Jingoo
open Jg_types

let sublist l start size =
(* FIXME: How can I raise an error here? for start < 0.
  Should I put this in Util ? *)
  let rec get_start l start =
    match l with
      _ :: tl -> if start > 0 then get_start tl (start - 1) else tl
    | _ -> []
  in
  Util.reduce_list size (get_start l start)

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
        (* FIXME: stop checking is_empty_name when possible *)
        (* pages are accessed with an index 'pg' and display a specified 'sz' number of persons. *)
        (fun _self conf base -> 
        let rec insert_person_inplace comp li person =
          let compare_persons p1 p2 =
            let res_surname =
              Gutil.alphabetic (Ezgw.Person.surname base p1) (Ezgw.Person.surname base p2) in
            if res_surname != 0 then res_surname
            else let res_firstname =
              Gutil.alphabetic (Ezgw.Person.first_name base p1) (Ezgw.Person.first_name base p2) in
              if res_firstname != 0 then res_firstname
              else Ezgw.Person.occ p1 - Ezgw.Person.occ p2
          in
          match li with 
            hd :: tl -> if compare_persons hd person <= 0
              then (person) :: hd :: tl
              else hd :: insert_person_inplace comp tl (person)
          | [] -> [person]
        in
        let person_collection = Gwdb.Collection.fold
          (fun li p ->
            if (Util.is_empty_name p) then li
            else insert_person_inplace Gutil.alphabetic li p) [] (Gwdb.persons base)
        in
        let access_person p = Data.get_n_mk_person conf base (Gwdb.get_iper p) in
        let page_num = Opt.default 0 @@ Util.p_getint conf.env "pg" in
        let page_size = Opt.default 50 @@ Util.p_getint conf.env "sz" in
        let person_list = List.fold_left (fun li p -> (access_person p) :: li) [] person_collection
        in
        let person_to_display = sublist person_list (page_num * page_size) (page_size) in
        let models = ("person_list", Tlist person_to_display)
          :: ("page_num", Tint page_num)
          :: ("page_size", Tint page_size)
          :: Data.default_env conf base
        in
        Interp.render ~conf ~file:"list_ind" ~models
        ) self conf base

      | _ -> self.RequestHandler.incorrect_request self conf base
    end

  }
