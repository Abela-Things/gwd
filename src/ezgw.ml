(* /!\ This is mostly copy/paste of the Perso module /!\ *)
(* Sync with perso from ed7525bac *)

open Geneweb

module MLink = Api_link_tree_piqi
open Config
open Def
open Gwdb
open Util
module Date2 = Date

type fam = (ifam * family * (iper * iper * iper) * bool)

type rel = (relation * person option)

type env =
  { all_gp : Perso.generation_person list option
  ; baseprefix : string option
  ; desc_level_table : (int array * int array) Lazy.t option
  ; desc_mark : bool array ref option
  ; f_link : bool option
  ; fam : fam option
  ; fam_link : fam option
  ; p_link : bool option
  ; prev_fam : fam option
  ; sosa : (iper * (Sosa.t * person) option) list ref option
  ; sosa_ref : person option Lazy.t option
  ; src : string option
  ; t_sosa : Perso.sosa_t option
  }

let conf_w_baseprefix conf env =
  match env.baseprefix with
  | Some baseprefix -> { conf with command = baseprefix }
  | None -> conf

let empty = { all_gp = None
            ; baseprefix = None
            ; desc_level_table = None
            ; desc_mark = None
            ; fam = None
            ; f_link = None
            ; fam_link = None
            ; p_link = None
            ; prev_fam = None
            ; sosa = None
            ; sosa_ref = None
            ; src = None
            ; t_sosa = None
            }

let env = empty

let get_env x = match x with Some x -> x | None -> raise Not_found

let get_sosa conf base env r p =
  try List.assoc (get_key_index p) !r
  with Not_found ->
    let s =
      match env.sosa_ref with
      | None -> None
      | Some v -> match env.t_sosa with
        | Some t_sosa -> Perso.find_sosa conf base p v t_sosa
        | None -> None
    in
    r := (get_key_index p, s) :: !r; s

let mk_note conf base p note =
  (* FIXME WTF IS THAT? *)
  let env = ['i', (fun () -> Util.default_image_name base p)] in
  let s = sou base note in
  let s = string_with_macros conf env s in
  let lines = Wiki.html_of_tlsw conf s in
  let wi =
    {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
     Wiki.wi_file_path = Notes.file_path conf base;
     Wiki.wi_person_exists = person_exists conf base;
     Wiki.wi_always_show_link = conf.wizard || conf.friend}
  in
  Wiki.syntax_links conf wi (String.concat "\n" lines)

let sex_of_index = function
  | 0 -> Male
  | 1 -> Female
  | 2 -> Neuter
  | _ -> raise (Invalid_argument "sex_of_index")

module Person = struct

  let access conf base p =
    Util.acces conf base p

  let age conf p =
    match Adef.od_of_cdate (get_birth p), get_death p with
    | Some (Dgreg (d, _)), NotDead ->
      CheckItem.time_elapsed d conf.today
    | _ -> raise Not_found

  let alias base p =
    match get_aliases p with
      nn :: _ -> sou base nn
    | _ -> raise Not_found

  let aliases base p =
    List.map (sou base) (get_aliases p)

  let approx_birth_place conf base p =
    snd (Util.get_approx_birth_date_place conf base p)

  let approx_death_place conf base p =
    snd (Util.get_approx_death_date_place conf base p)

  let auto_image_file_name conf base p =
    match auto_image_file conf base p with
    | Some s -> s
    | _ -> raise Not_found

  let bname_prefix conf = Util.commd conf

  let birth_date p = Adef.od_of_cdate (get_birth p)

  let birth_place conf base p =
    Util.string_of_place conf (sou base (get_birth_place p))

  let birth_note conf base p =
    mk_note conf base p (get_birth_note p)

  let baptism_date p = Adef.od_of_cdate (get_baptism p)

  let baptism_place conf base p =
    Util.string_of_place conf (sou base (get_baptism_place p))

  let baptism_note conf base p =
    mk_note conf base p (get_baptism_note p)

  let burial p =
    get_burial p

  let burial_place conf base p =
    Util.string_of_place conf (sou base (get_burial_place p))

  let burial_note conf base p =
    mk_note conf base p (get_burial_note p)

  let children base p = ChangeChildren.select_children_of base p

  (* let child conf base p =
   *   match get_env "child" env with
   *     Vind p when mode_local env ->
   *     let auth = authorized_age conf base p in
   *     let ep = p, auth in eval_person_field_var conf base env ep loc sl *)

  let child_name conf base p =
    let force_surname =
      match get_parents p with
        None -> false
      | Some ifam ->
        p_surname base (pget conf base (get_father (foi base ifam))) <>
        p_surname base p
    in
    if force_surname then person_text conf base p
    else person_text_no_surn_no_acc_chk conf base p

  let consanguinity p =
    let c = get_consang p in
    if c != Adef.fix (-1) && c >= Adef.fix_of_float 0.0001
    then Adef.float_of_fix c
    else 0.

  let cremation_place conf base p =
    Util.string_of_place conf (sou base (get_burial_place p))

  let dates conf base p =
    Date.short_dates_text conf base p

  let death p =
    get_death p

  (* FIXME *)
  let death_age conf p =
    match Date.get_birth_death_date p with
    | Some (Dgreg (({prec = Sure | About | Maybe ; _} as d1), _)),
      Some (Dgreg (({prec = Sure | About | Maybe ; _} as d2), _)), approx
      when d1 <> d2 ->
      let a = CheckItem.time_elapsed d1 d2 in
      let s =
        if not approx && d1.prec = Sure && d2.prec = Sure then ""
        else transl_decline conf "possibly (date)" "" ^ " "
      in
      s ^ Date.string_of_age conf a
    | _ -> ""

  let death_place conf base p =
    Util.string_of_place conf (sou base (get_death_place p))

  let death_note conf base p =
    mk_note conf base p (get_death_note p)

  let died conf p =
    Perso.string_of_died conf p true

  let digest base p =
    Update.digest_person (UpdateInd.string_person_of base p)

  let events conf base p =
    Perso.events_list conf base p

  let father_age_at_birth conf base p =
    Perso.string_of_parent_age conf base (p, true) get_father

  let first_name base p =
    p_first_name base p

  let first_name_aliases base p =
    List.map (sou base) (get_first_names_aliases p)

  let first_name_key base p =
    code_varenv (Name.lower (p_first_name base p))

  let first_name_key_val base p =
    Name.lower (p_first_name base p)

  let first_name_key_strip base p =
    Name.strip_c (p_first_name base p) '"'

  let nb_families conf p =
    match env.p_link with
    | Some _ ->
      List.length (Perso_link.get_family_correspondance conf.command (get_key_index p))
    | _ ->
      Array.length (get_family p)


  let history_file base p =
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let occ = get_occ p in History_diff.history_file fn sn occ

  let image base p =
    sou base (get_image p)

  let image_html_url conf base p =
    Perso.string_of_image_url conf base (p, true) true

  let image_size conf base p =
    Perso.string_of_image_size conf base (p, true)

  let image_medium_size conf base p =
    Perso.string_of_image_medium_size conf base (p, true)

  let image_small_size conf base p =
    Perso.string_of_image_small_size conf base (p, true)

  let image_url conf base p =
    Perso.string_of_image_url conf base (p, true) false

  let ind_access p =
    (* deprecated since 5.00: rather use "i=%index;" *)
    "i=" ^ string_of_int (Adef.int_of_iper (get_key_index p))

  let is_accessible_by_key conf base p =
    Util.accessible_by_key
      conf base p (p_first_name base p) (p_surname base p)

  let is_birthday conf p =
    match Adef.od_of_cdate (get_birth p) with
    | Some (Dgreg (d, _)) ->
      if d.prec = Sure && get_death p = NotDead then
        d.day = conf.today.day && d.month = conf.today.month &&
        d.year < conf.today.year ||
        not (CheckItem.leap_year conf.today.year) && d.day = 29 &&
        d.month = 2 && conf.today.day = 1 && conf.today.month = 3
      else false
    | _ -> false


  let linked_page conf base p s =
    let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
    let fname = Filename.concat bdir "notes_links" in
    let db = NotesLinks.read_db_from_file fname in
    let db = Notes.merge_possible_aliases conf db in
    let key =
      let fn = Name.lower (sou base (get_first_name p)) in
      let sn = Name.lower (sou base (get_surname p)) in
      fn, sn, get_occ p
    in
    List.fold_left (Perso.linked_page_text conf base p s key) "" db

  let mark_descendants conf base p =
    let r = get_env env.desc_mark in
    let tab = Array.make (nb_of_persons base) false in
    let rec mark_descendants len p =
      let i = Adef.int_of_iper (get_key_index p) in
      if tab.(i) then ()
      else
        begin
          tab.(i) <- true;
          let u = p in
          for i = 0 to Array.length (get_family u) - 1 do
            let des = foi base (get_family u).(i) in
            for i = 0 to Array.length (get_children des) - 1 do
              mark_descendants (len + 1)
                (pget conf base (get_children des).(i))
            done
          done
        end
    in
    mark_descendants 0 p ;
    r := tab; ""

  let marriage_age conf p =
    let (_, fam, _, m_auth) = get_env env.fam in
    if m_auth then
      match
        Adef.od_of_cdate (get_birth p),
        Adef.od_of_cdate (get_marriage fam)
      with
        Some (Dgreg (({prec = Sure | About | Maybe ; _} as d1), _)),
        Some (Dgreg (({prec = Sure | About | Maybe ; _} as d2), _)) ->
        let a = CheckItem.time_elapsed d1 d2 in
        Date.string_of_age conf a
      | _ -> ""
    else ""

  let max_ancestor_level conf base p =
    let emal =
      match p_getint conf.env "v" with
        Some i -> i
      | None -> 120
    in
    Perso.max_ancestor_level conf base (get_key_index p) emal + 1

  let mother_age_at_birth conf base p =
    Perso.string_of_parent_age conf base (p, true) get_mother

  (* FIXME *)
  let misc_names conf base p =
    let list = Gwdb.person_misc_names base p (Util.nobtit conf base) in
    let first_name = p_first_name base p in
    let surname = p_surname base p in
    if first_name <> "?" && surname <> "?" then
      Name.lower (first_name ^ " " ^ surname) :: list
    else list

  let nb_children_total base p =
    let n =
      List.fold_left
        (fun n ifam -> n + Array.length (get_children (foi base ifam))) 0
        (Array.to_list (get_family p))
    in
    string_of_int n

  let nb_children conf base p =
    match env.fam with
    | Some (_, fam, _, _) ->
      string_of_int (Array.length (get_children fam))
    | None ->
      match env.fam_link with
      | Some (ifam, _, _, _) ->
        let conf =
          match env.baseprefix with
          | Some baseprefix -> { conf with command = baseprefix }
          | None -> conf
        in
        let children =
          Perso_link.get_children_of_fam conf.command ifam
        in
        string_of_int (List.length children)
      | None ->
        let n =
          List.fold_left
            (fun n ifam ->
               n + Array.length (get_children (foi base ifam)))
            0 (Array.to_list (get_family p))
        in
        string_of_int n

  let nobility_titles conf base p = Util.nobtit conf base p

  let notes conf base p =
    if not conf.no_note then
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      let s = sou base (get_notes p) in
      let s = string_with_macros conf env s in
      let lines = Wiki.html_of_tlsw conf s in
      let wi =
        {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      let s = Wiki.syntax_links conf wi (String.concat "\n" lines) in
      if conf.pure_xhtml then Util.check_xhtml s else s
    else ""

  let occ p =
    get_occ p

  let occupation conf base p =
    let s = sou base (get_occupation p) in
    let s =
      let wi =
        {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      Wiki.syntax_links conf wi s
    in
    string_with_macros conf [] s

  let on_baptism_date conf ?(long_date = false) p =
    match Adef.od_of_cdate (get_baptism p) with
    | Some d ->
      if long_date then Date.string_of_ondate conf d ^ Date.get_wday conf d
      else Date.string_of_ondate conf d
    | _ -> raise Not_found

  let on_birth_date conf ?(long_date = false) p =
    match Adef.od_of_cdate (get_birth p) with
    | Some d ->
      if long_date then Date.string_of_ondate conf d ^ Date.get_wday conf d
      else Date.string_of_ondate conf d
    | _ -> raise Not_found

  let parents p = get_parents p

  let pnote = notes

  let rparents p = get_rparents p

  let related conf base p =
    List.sort
      (fun (c1, _) (c2, _) ->
         let mk_date c =
           match Adef.od_of_cdate (get_baptism c) with
           | None -> Adef.od_of_cdate (get_birth c)
           | x -> x
         in
         match mk_date c1, mk_date c2 with
         | Some d1, Some d2 -> if CheckItem.strictly_before d1 d2 then -1 else 1
         | _ -> -1)
    @@
    List.fold_left (fun list ic ->
        let c = pget conf base ic in
        List.fold_left (fun acc r -> match r.r_fath, r.r_moth with
            | Some ip, _  when ip = get_key_index p -> (c, r) :: acc
            | _ , Some ip when ip = get_key_index p -> (c, r) :: acc
            | _ -> acc)
          list (get_rparents c) )
      [] (List.sort_uniq compare (get_related p))

  (* Why isnt this already unique? *)
  let relations p =
    List.sort_uniq compare (get_related p)

  let siblings base p =
    match get_parents p with
    | Some ifam ->
      let ip = get_key_index p in
      Array.fold_right
        (fun i acc -> if i <> ip then i :: acc else acc)
        (get_children (foi base ifam))
        []
    | None -> []

  let half_siblings base p =
    match get_parents p with
    | Some ifam ->
      let ip = get_key_index p in
      let f = foi base ifam in
      let fath = poi base @@ get_father f in
      let moth = poi base @@ get_mother f in
      let filter = fun (acc : iper list) i ->
        if i = ifam then acc else
          Array.fold_right
            (fun i acc -> if i <> ip then i :: acc else acc)
            (get_children (foi base i)) acc
      in
      Array.fold_left
        filter (Array.fold_left filter [] (get_family fath)) (get_family moth)
    | None -> []

  let slash_baptism_date conf p =
    match Adef.od_of_cdate (get_baptism p) with
    | Some d -> Date.string_slash_of_date conf d
    | _ -> ""

  let slash_birth_date conf p =
    match Adef.od_of_cdate (get_birth p) with
    | Some d -> Date.string_slash_of_date conf d
    | _ -> ""

  let slash_approx_birth_date conf base p =
    match fst (Util.get_approx_birth_date_place conf base p) with
    | Some d -> Date.string_slash_of_date conf d
    | _ -> ""

  let static_max_ancestor_level conf base p =
    Perso.max_ancestor_level conf base (get_key_index p) 120 + 1

  (* FIXME *)
  let on_burial_date conf p =
    match get_burial p with
    | Buried cod ->
      begin match Adef.od_of_cdate cod with
        | Some d ->
          begin match p_getenv conf.base_env "long_date" with
              Some "yes" ->
              Date.string_of_ondate conf d ^ Date.get_wday conf d
            | _ -> Date.string_of_ondate conf d
          end
        | _ -> ""
      end
    | _ -> raise Not_found

  let psources conf base p =
    if not conf.no_note then
      let env = ['i', (fun () -> Util.default_image_name base p)] in
      let s = sou base (get_psources p) in
      let s = string_with_macros conf env s in
      let lines = Wiki.html_of_tlsw conf s in
      let wi =
        {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      let s = Wiki.syntax_links conf wi (String.concat "\n" lines) in
      if conf.pure_xhtml then Util.check_xhtml s else s
    else ""

  let slash_burial_date conf p =
    match get_burial p with
    | Buried cod ->
      begin match Adef.od_of_cdate cod with
        | Some d -> Date.string_slash_of_date conf d
        | _ -> ""
      end
    | _ -> raise Not_found

  let on_cremation_date conf p =
    match get_burial p with
      Cremated cod ->
      begin match Adef.od_of_cdate cod with
        | Some d ->
          begin match p_getenv conf.base_env "long_date" with
              Some "yes" ->
              Date.string_of_ondate conf d ^ Date.get_wday conf d
            | _ -> Date.string_of_ondate conf d
          end
        | _ -> ""
      end
    | _ -> raise Not_found

  let slash_cremation_date conf p =
    match get_burial p with
    | Cremated cod ->
      begin match Adef.od_of_cdate cod with
        | Some d -> Date.string_slash_of_date conf d
        | _ -> ""
      end
    | _ -> raise Not_found

  let on_death_date conf p =
    match get_death p with
    | Death (_, d) ->
      let d = Adef.date_of_cdate d in
      begin match p_getenv conf.base_env "long_date" with
          Some "yes" -> Date.string_of_ondate conf d ^ Date.get_wday conf d
        | _ -> Date.string_of_ondate conf d
      end
    | _ -> ""

  let slash_death_date conf p =
    match get_death p with
    | Death (_, d) ->
      Date.string_slash_of_date conf (Adef.date_of_cdate d)
    | _ -> ""

  let slash_approx_death_date conf base p =
    match fst (Util.get_approx_death_date_place conf base p) with
    | Some d -> Date.string_slash_of_date conf d
    | _ -> ""

  let prev_fam_father _p =
    let (_, _, (ifath, _, _), _) = get_env env.prev_fam in
    string_of_int (Adef.int_of_iper ifath)

  let prev_fam_index _p =
    let (ifam, _, _, _) = get_env env.prev_fam in
    string_of_int (Adef.int_of_ifam ifam)

  let prev_fam_mother _p =
    let (_, _, (_, imoth, _), _) = get_env env.prev_fam in
    string_of_int (Adef.int_of_iper imoth)

  let public_name base p =
    sou base (get_public_name p)

  let qualifier base p =
    match get_qualifiers p with
    | nn :: _ -> sou base nn
    | _ -> ""

  let qualifiers base p =
    List.map (sou base) (get_qualifiers p)

  let sex p =
    index_of_sex (get_sex p)

  let sosa conf base env r p =
    get_sosa conf base env r p

  let sosa_in_list p =
    let all_gp = get_env env.all_gp in
    match Perso.get_link all_gp (get_key_index p) with
      Some (Perso.GP_person (s, _, _)) -> Sosa.to_string s
    | _ -> ""

  let sosa_link conf base p =
    let x = get_env env.sosa in
    match get_sosa conf base env x p with
    | Some (n, q) ->
      Printf.sprintf "m=RL;i1=%d;i2=%d;b1=1;b2=%s"
        (Adef.int_of_iper (get_key_index p))
        (Adef.int_of_iper (get_key_index q)) (Sosa.to_string n)
    | None -> ""

  let source conf base p =
    let s = get_env env.src in
    let env = ['i', (fun () -> Util.default_image_name base p)] in
    let s =
      let wi =
        {Wiki.wi_mode = "NOTES";
         Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      Wiki.syntax_links conf wi s
    in
    string_with_macros conf env s

  let __src_aux get conf base p =
    Array.map
      (fun ifam ->
         let fam = foi base ifam in
         let isp = Gutil.spouse (get_key_index p) fam in
         if authorized_age conf base (poi base isp) then sou base (get fam)
         else "")
      (get_family p)

  let source_baptism base p =
    sou base (get_baptism_src p)

  let source_birth base p =
    sou base (get_birth_src p)

  let source_burial base p =
    sou base (get_burial_src p)

  let source_death base p =
    sou base (get_death_src p)

  let source_fsource =
    __src_aux get_fsources

  let source_marriage =
    __src_aux get_marriage_src

  let source_psources base p =
    sou base (get_psources p)

  let surname base p =
    p_surname base p

  let surname_aliases base p =
    List.map (sou base) (get_surnames_aliases p)

  let surname_key base p =
    code_varenv (Name.lower (p_surname base p))

  let surname_key_val base p =
    Name.lower (p_surname base p)

  let surname_key_strip base p =
    Name.strip_c (p_surname base p) '"'

end

module Date = struct

  let prec conf = function
    | Dgreg (dmy, _) -> Date.prec_text conf dmy
    | _ ->  ""

  let day = function
    | Dgreg (dmy, _) when dmy.day <> 0 -> string_of_int dmy.day
    | _ -> ""

  let day2 = function
    | Dgreg (dmy, _) ->
      begin match dmy.prec with
        | (OrYear dmy2 | YearInt dmy2) when dmy2.day2 <> 0 ->
          string_of_int dmy2.day2
        | _ ->  ""
      end
    | _ -> ""

  let julian_day = function
    | Dgreg (dmy, _) -> string_of_int (Calendar.sdn_of_julian dmy)
    | _ -> ""

  let month = function
    | Dgreg (dmy, _) -> Date.month_text dmy
    | _ -> ""

  let month2 = function
    | Dgreg (dmy, _) ->
      begin match dmy.prec with
        | (OrYear dmy2 | YearInt dmy2) when dmy2.month2 <> 0 ->
          string_of_int dmy2.month2
        | _ -> ""
      end
    | _ -> ""

  let year = function
    | Dgreg (dmy, _) -> string_of_int dmy.year
    | _ -> ""

  let year2 = function
    | Dgreg (dmy, _) ->
      begin match dmy.prec with
        | (OrYear dmy2 | YearInt dmy2) -> string_of_int dmy2.year2
        | _ -> ""
      end
    | _ -> ""

  let date conf d =
    Date.string_of_date_sep conf "<br/>" d

end

type rel_ = (int * relation_type * iper * bool)

module Related = struct

  let type_ conf (i, rt, _, is_relation) =
    if is_relation then relation_type_text conf rt i
    else rchild_type_text conf rt i

  let iper (_, _, ip, _) = ip

end

module Relation = struct

  let has_relation_her = function
    | ({ r_moth = Some _ ; _}, None) -> true
    | _ -> false

  let has_relation_him = function
    | ({ r_fath = Some _ ; _}, None) -> true
    | _ -> false

  let related : _ -> rel_ = function
    | ({ r_type = rt ; _ }, Some p) ->
      (index_of_sex (get_sex p), rt, get_key_index p, false)
    | _ -> raise Not_found

  let related_type conf = function
    | (r, Some c) ->
      rchild_type_text conf r.r_type (index_of_sex (get_sex c))
    | _ -> raise Not_found

  let relation_type conf = function
    | (r, None) ->
      begin match r.r_fath, r.r_moth with
          Some _, None -> relation_type_text conf r.r_type 0
        | None, Some _ -> relation_type_text conf r.r_type 1
        | Some _, Some _ -> relation_type_text conf r.r_type 2
        | _ -> raise Not_found
      end
    | _ -> raise Not_found

  let relation_her = function
    |  ({r_moth = Some ip; r_type = rt ; _}, None) ->  (1, rt, ip, true)
    | _ -> raise Not_found

  let relation_him = function
    | ({r_fath = Some ip; r_type = rt ; _}, None) -> (0, rt, ip, true)
    | _ -> raise Not_found

end

module Family = struct

  let are_divorced (_, fam, _, _) =
    match get_divorce fam with Divorced _ -> true | _ -> false

  let are_engaged (_, fam, _, _) =
    get_relation fam = Engaged

  let are_married (_, fam, _, _) =
    get_relation fam = Married || get_relation fam = NoSexesCheckMarried

  let are_not_married (_, fam, _, _) =
    get_relation fam = NotMarried ||
    get_relation fam = NoSexesCheckNotMarried

  let are_separated (_, fam, _, _) =
    get_divorce fam = Separated

  let children (_, fam, _, _) = get_children fam

  let desc_level (ifam, _, _, _) =
    let levt = get_env env.desc_level_table in
    let (_, flevt) = Lazy.force levt in
    string_of_int flevt.(Adef.int_of_ifam ifam)

  let divorce_date conf (_, fam, _, m_auth) =
    match get_divorce fam with
    | Divorced d ->
      begin match Adef.od_of_cdate d with
        | Some d when m_auth && p_getenv conf.base_env "long_date" = Some "yes" ->
          Date2.string_of_ondate conf d ^ Date2.get_wday conf d
        | Some d when m_auth -> Date2.string_of_ondate conf d
        | _ -> ""
      end
    | _ -> raise Not_found

  let father (_, _, (ifath, _, _), _) =
    if env.f_link = None then ifath else raise Not_found

  let ifam (ifam, _, _, _) =
    Adef.int_of_ifam ifam

  let is_no_mention (_, fam, _, _) =
    get_relation fam = NoMention

  let is_no_sexes_check (_, fam, _, _) =
    get_relation fam = NoSexesCheckNotMarried ||
    get_relation fam = NoSexesCheckMarried

  let marriage_date (_, fam, (_, _, _), m_auth) =
    if m_auth then Adef.od_of_cdate (get_marriage fam) else None

  (* FIXME: string_of_place was called but might not be useful here *)
  let marriage_place base (_, fam, _, m_auth) =
    if m_auth then sou base (get_marriage_place fam) else ""

  (* FIXME? *)
  let marriage_note conf base (_, fam, _, m_auth) =
    if m_auth && not conf.no_note then
      let s = sou base (get_marriage_note fam) in
      let s = string_with_macros conf [] s in
      let lines = Wiki.html_of_tlsw conf s in
      let wi =
        {Wiki.wi_mode = "NOTES";
         Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      Wiki.syntax_links conf wi (String.concat "\n" lines)
    else ""

  let marriage_source conf base (_, fam, _, m_auth) =
    if m_auth then
      let s = sou base (get_marriage_src fam) in
      let s = string_with_macros conf [] s in
      let lines = Wiki.html_of_tlsw conf s in
      let wi =
        {Wiki.wi_mode = "NOTES";
         Wiki.wi_cancel_links = conf.cancel_links;
         Wiki.wi_file_path = Notes.file_path conf base;
         Wiki.wi_person_exists = person_exists conf base;
         Wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      Wiki.syntax_links conf wi (String.concat "\n" lines)
    else ""

  let mother (_, _, (_, imoth, _), _) =
    if env.f_link = None then imoth else raise Not_found

  let nb_children (_, fam, _, _) =
    Array.length (get_children fam)

  let on_marriage_date conf (_, fam, _, m_auth) =
    if m_auth then
      match Adef.od_of_cdate (get_marriage fam) with
      | Some s ->
        begin match p_getenv conf.base_env "long_date" with
          | Some "yes" ->
            Date2.string_of_ondate conf s ^ Date2.get_wday conf s
          | _ -> Date2.string_of_ondate conf s
        end
      | _ -> raise Not_found
    else raise Not_found

  let origin_file conf base (_, fam, _, _) =
    if conf.wizard then sou base (get_origin_file fam)
    else raise Not_found

  let set_infinite_desc_level (ifam, _, _, _) =
    let levt = get_env env.desc_level_table in
    let (_, flevt) = Lazy.force levt in
    flevt.(Adef.int_of_ifam ifam) <- Perso.infinite; ""

  let spouse_iper (_, _, (_, _, ip), _) = ip

  let witnesses (_, fam, _, m_auth) =
    if m_auth then get_witnesses fam else raise Not_found

end

module Event = struct

  let date (_, d, _, _, _, _, _) =
    Adef.od_of_cdate d

  let place conf base (_, _, p, _, _, _, _) =
    Util.string_of_place conf (sou base p)

  let spouse_opt (_, _, _, _, _, _, isp) =
    isp

  let src base (_, _, _, _, s, _, _) =
    sou base s

  let kind base (n, _, _, _, _, _, _) =
    match n with
    | Geneweb.Perso.Pevent Epers_Birth -> "EPERS_BIRTH"
    | Pevent Epers_Baptism -> "EPERS_BAPTISM"
    | Pevent Epers_Death -> "EPERS_DEATH"
    | Pevent Epers_Burial -> "EPERS_BURIAL"
    | Pevent Epers_Cremation -> "EPERS_CREMATION"
    | Pevent Epers_Accomplishment -> "EPERS_ACCOMPLISHMENT"
    | Pevent Epers_Acquisition -> "EPERS_ACQUISITION"
    | Pevent Epers_Adhesion -> "EPERS_ADHESION"
    | Pevent Epers_BaptismLDS -> "EPERS_BAPTISMLDS"
    | Pevent Epers_BarMitzvah -> "EPERS_BARMITZVAH"
    | Pevent Epers_BatMitzvah -> "EPERS_BATMITZVAH"
    | Pevent Epers_Benediction -> "EPERS_BENEDICTION"
    | Pevent Epers_ChangeName -> "EPERS_CHANGENAME"
    | Pevent Epers_Circumcision -> "EPERS_CIRCUMCISION"
    | Pevent Epers_Confirmation -> "EPERS_CONFIRMATION"
    | Pevent Epers_ConfirmationLDS -> "EPERS_CONFIRMATIONLDS"
    | Pevent Epers_Decoration -> "EPERS_DECORATION"
    | Pevent Epers_DemobilisationMilitaire -> "EPERS_DEMOBILISATIONMILITAIRE"
    | Pevent Epers_Diploma -> "EPERS_DIPLOMA"
    | Pevent Epers_Distinction -> "EPERS_DISTINCTION"
    | Pevent Epers_Dotation -> "EPERS_DOTATION"
    | Pevent Epers_DotationLDS -> "EPERS_DOTATIONLDS"
    | Pevent Epers_Education -> "EPERS_EDUCATION"
    | Pevent Epers_Election -> "EPERS_ELECTION"
    | Pevent Epers_Emigration -> "EPERS_EMIGRATION"
    | Pevent Epers_Excommunication -> "EPERS_EXCOMMUNICATION"
    | Pevent Epers_FamilyLinkLDS -> "EPERS_FAMILYLINKLDS"
    | Pevent Epers_FirstCommunion -> "EPERS_FIRSTCOMMUNION"
    | Pevent Epers_Funeral -> "EPERS_FUNERAL"
    | Pevent Epers_Graduate -> "EPERS_GRADUATE"
    | Pevent Epers_Hospitalisation -> "EPERS_HOSPITALISATION"
    | Pevent Epers_Illness -> "EPERS_ILLNESS"
    | Pevent Epers_Immigration -> "EPERS_IMMIGRATION"
    | Pevent Epers_ListePassenger -> "EPERS_LISTEPASSENGER"
    | Pevent Epers_MilitaryDistinction -> "EPERS_MILITARYDISTINCTION"
    | Pevent Epers_MilitaryPromotion -> "EPERS_MILITARYPROMOTION"
    | Pevent Epers_MilitaryService -> "EPERS_MILITARYSERVICE"
    | Pevent Epers_MobilisationMilitaire -> "EPERS_MOBILISATIONMILITAIRE"
    | Pevent Epers_Naturalisation -> "EPERS_NATURALISATION"
    | Pevent Epers_Occupation -> "EPERS_OCCUPATION"
    | Pevent Epers_Ordination -> "EPERS_ORDINATION"
    | Pevent Epers_Property -> "EPERS_PROPERTY"
    | Pevent Epers_Recensement -> "EPERS_RECENSEMENT"
    | Pevent Epers_Residence -> "EPERS_RESIDENCE"
    | Pevent Epers_Retired -> "EPERS_RETIRED"
    | Pevent Epers_ScellentChildLDS -> "EPERS_SCELLENTCHILDLDS"
    | Pevent Epers_ScellentParentLDS -> "EPERS_SCELLENTPARENTLDS"
    | Pevent Epers_ScellentSpouseLDS -> "EPERS_SCELLENTSPOUSELDS"
    | Pevent Epers_VenteBien -> "EPERS_VENTEBIEN"
    | Pevent Epers_Will -> "EPERS_WILL"
    | Fevent Efam_Marriage -> "EFAM_MARRIAGE"
    | Fevent Efam_NoMarriage -> "EFAM_NO_MARRIAGE"
    | Fevent Efam_NoMention -> "EFAM_NO_MENTION"
    | Fevent Efam_Engage -> "EFAM_ENGAGE"
    | Fevent Efam_Divorce -> "EFAM_DIVORCE"
    | Fevent Efam_Separated -> "EFAM_SEPARATED"
    | Fevent Efam_Annulation -> "EFAM_ANNULATION"
    | Fevent Efam_MarriageBann -> "EFAM_MARRIAGE_BANN"
    | Fevent Efam_MarriageContract -> "EFAM_MARRIAGE_CONTRACT"
    | Fevent Efam_MarriageLicense -> "EFAM_MARRIAGE_LICENSE"
    | Fevent Efam_PACS -> "EFAM_PACS"
    | Fevent Efam_Residence -> "EFAM_RESIDENCE"
    | Pevent Epers_Name s -> sou base s
    | Fevent Efam_Name s -> sou base s

  let name conf base (n, _, _, _, _, _, _) =
    match n with
    | Geneweb.Perso.Pevent name -> Util.string_of_pevent_name conf base name
    | Fevent name -> Util.string_of_fevent_name conf base name

  let note base (_, _, _, n, _, _, _) =
    sou base n

  let witnesses (_, _, _, _, _, w, _) =
    w

end
