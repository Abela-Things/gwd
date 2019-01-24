(* TODO: extract useful conf fields and marshal when possible (espacially translation / date) *)

(* TODO:
   Do not use on_xxx_date: use dates and use template to display it.
 *)

open Geneweb
open Jingoo
open Jg_types

let mk_opt fn = function None -> Tnull | Some x -> fn x

let date_compare_aux =
  let compare field d1 d2 =
    Jg_runtime.jg_compare
      (Jg_runtime.jg_obj_lookup d1 field)
      (Jg_runtime.jg_obj_lookup d2 field)
  in
  fun d1 d2 ->
    match unbox_int @@ compare "year" d1 d2 with
    | 0 -> begin match unbox_int @@ compare "month" d1 d2 with
        | 0 -> begin match unbox_int @@ compare "day" d1 d2 with
            | 0 -> begin match Jg_runtime.jg_obj_lookup d1 "prec", Jg_runtime.jg_obj_lookup d2 "prec" with
                | p1, p2 when p1 = p2 -> Tint 0
                | Tstr "before", _ -> Tint (-1)
                | Tstr "after", _ -> Tint 1
                | _ -> Tint 0
              end
            | c -> Tint c
          end
        | c -> Tint c
      end
    | c -> Tint c

let rec mk_family (conf : Config.config) base fcd =
  let module E = Ezgw.Family in
  let get wrap fn = try wrap (fn fcd) with Not_found -> Tnull in
  let get_str = get box_string in
  let get_bool = get box_bool in
  let get_int = get box_int in
  let f = E.father fcd in
  let m = E.mother fcd in
  let divorce_date = get_str (E.divorce_date conf) in
  let father = Tlazy (lazy (get_n_mk_person conf base f) ) in
  let mother = Tlazy (lazy (get_n_mk_person conf base m) ) in
  let spouse =
    let (_, _, (ifath, imoth, ispouse), _) = fcd in
    if ifath = ispouse then father
    else if imoth = ispouse then mother
    else Tnull
  in
  let children =
    Tlazy (lazy (Tarray (Array.map (get_n_mk_person conf base) (E.children fcd)) ) )
  in
  let marriage_date = mk_opt (mk_date conf) (E.marriage_date fcd) in
  let marriage_place = get_str (E.marriage_place base) in
  let marriage_note = get_str (E.marriage_note conf base) in
  let marriage_source = get_str (E.marriage_source conf base) in
  let on_marriage_date = get_str (E.on_marriage_date conf) in
  let are_divorced = get_bool E.are_divorced in
  let are_separated = get_bool E.are_separated in
  let are_married = get_bool E.are_married in
  let are_engaged = get_bool E.are_engaged in
  let are_not_married = get_bool E.are_not_married in
  let is_no_mention = get_bool E.is_no_mention in
  let is_no_sexes_check = get_bool E.is_no_sexes_check in
  let ifam = get_int E.ifam in
  let witnesses = match E.witnesses fcd with
    | [||] -> Tarray [||]
    | w -> Tlazy (lazy (box_array @@ Array.map (get_n_mk_person conf base) w))
  in
  let origin_file = Tlazy (lazy (get_str (E.origin_file conf base))) in
  Tpat (function
      | "are_divorced" -> are_divorced
      | "are_married" -> are_married
      | "are_engaged" -> are_engaged
      | "are_not_married" -> are_not_married
      | "are_separated" -> are_separated
      | "divorce_date" -> divorce_date
      | "children" -> children
      | "father" -> father
      | "ifam" -> ifam
      | "is_no_mention" -> is_no_mention
      | "is_no_sexes_check" -> is_no_sexes_check
      | "marriage_date" -> marriage_date
      | "marriage_place" -> marriage_place
      | "marriage_note" -> marriage_note
      | "marriage_source" -> marriage_source
      | "mother" -> mother
      | "on_marriage_date" -> on_marriage_date
      | "origin_file" -> origin_file
      | "spouse" -> spouse
      | "witnesses" -> witnesses
      | _ -> raise Not_found
    )

and get_n_mk_family conf base ?(origin = Adef.iper_of_int (-1)) ifam cpl =
  let ifath = Gwdb.get_father cpl in
  let imoth = Gwdb.get_mother cpl in
  let cpl =
    ifath, imoth, (if ifath = origin then imoth
                   else if imoth = origin then ifath
                   else origin)
  in
  let m_auth =
    Util.authorized_age conf base (Util.pget conf base ifath)
    && Util.authorized_age conf base (Util.pget conf base imoth)
  in
  mk_family conf base (ifam, Gwdb.foi base ifam, cpl, m_auth)

and date_compare = func_arg2_no_kw date_compare_aux

and date_eq = func_arg2_no_kw (fun d1 d2 -> Tbool (date_compare_aux d1 d2 = Tint 0))

and mk_date conf d =
  let opt =
    match d with
    | Def.Dtext _ -> fun _ -> Tnull
    | Dgreg (d, c) -> fun fn -> fn d c
  in
  let year = opt (fun d _ -> Tint d.Def.year) in
  let month = opt (fun d _ -> Tint d.Def.month) in
  let day = opt (fun d _ -> Tint d.Def.day) in
  let prec = opt (fun d _ -> Tstr (match d.Def.prec with
      | Def.Sure -> "sure"
      | About -> "about"
      | Maybe -> "maybe"
      | Before -> "before"
      | After -> "after"
      | OrYear _ -> "oryear"
      | YearInt _ -> "yearint")
    )
  in
  let d2 = opt (fun d c -> match d.Def.prec with
      | OrYear d2 | YearInt d2 ->
        mk_date conf (Def.Dgreg ( { Def.day = d2.Def.day2
                                  ; month = d2.Def.month2
                                  ; year = d2.Def.year2
                                  ; prec = Def.Sure ; delta = 0 }
                                , c) )
      | _ -> Tnull )
  in
  let calendar =
    opt (fun _ -> function
        | Dgregorian -> Tstr "Dgregorian"
        | Djulian -> Tstr "Djulian"
        | Dfrench -> Tstr "Dfrench"
        | Dhebrew -> Tstr "Dhebrew")
  in
  Tpat (function
      | "calendar" -> calendar
      | "d2" -> d2
      | "day" -> day
      | "month" -> month
      | "prec" -> prec
      | "year" -> year
      | "__compare__" -> date_compare
      | "__eq__" -> date_eq
      | _ -> raise Not_found
    )

and to_dmy d =
  let int s = unbox_int (Jg_runtime.jg_obj_lookup d s) in
  { Def.day = int "day" ; month = int "month" ; year = int "year"
  ; prec = of_prec d
  ; delta = 0 }

and to_dmy2 d =
  let int s = unbox_int (Jg_runtime.jg_obj_lookup d s) in
  { Def.day2 = int "day" ; month2 = int "month" ; year2 = int "year"
  ; delta2 = 0 }

and to_prec = function
  | Def.Sure -> "sure"
  | About -> "about"
  | Maybe -> "maybe"
  | Before -> "before"
  | After -> "after"
  | OrYear _ -> "oryear"
  | YearInt _ -> "yearint"

and of_prec d = match Jg_runtime.jg_obj_lookup d "prec" with
  | Tstr "sure" -> Def.Sure
  | Tstr "about" -> About
  | Tstr "maybe" -> Maybe
  | Tstr "before" -> Before
  | Tstr "after" -> After
  | Tstr "oryear" -> OrYear (to_dmy2 d)
  | Tstr "yearint" -> YearInt (to_dmy2 d)
  | _ -> assert false

and to_gregorian_aux calendar d =
  let d = to_dmy d in
  match calendar with
  | "Dgregorian" -> d
  | "Djulian" -> Calendar.gregorian_of_julian d
  | "Dfrench" -> Calendar.gregorian_of_french d
  | "Dhebrew" -> Calendar.gregorian_of_hebrew d
  | x -> print_endline @@ Printf.sprintf "%s: %s" __LOC__ x ; assert false

and module_date conf =
  let now =
    Tvolatile (fun () ->
        let now = Unix.gmtime @@ Unix.time () in
        let day = Tint now.tm_mday in
        let month = Tint (now.tm_mon + 1) in
        let year = Tint (now.tm_year + 1900) in
        Tpat (function
            | "day" -> day
            | "month" -> month
            | "year" -> year
            | "prec" -> Tstr "sure"
            | _ -> raise Not_found)
      )
  in
  let death_symbol = Date.death_symbol conf in
  let string_of_ondate =
    func_arg1_no_kw @@ fun d ->
    Tlazy (lazy (Tstr (Date.string_of_ondate conf @@ Def.Dgreg (to_dmy d, Def.Dgregorian) ) ) )
  in
  let code_french_year =
    func_arg1_no_kw (fun i -> box_string @@ Date.code_french_year conf (unbox_int i))
  in
  let string_of_age =
    func_arg1_no_kw (fun d -> box_string @@ Date.string_of_age conf (to_dmy d) )
  in
  Tpat (function
      | "calendar" -> func_arg2_no_kw (fun dst d ->
          (* let src = unbox_string @@ Jg_runtime.jg_obj_lookup d "calendar" in *)
          let convert fn = mk_dmy @@ fn @@ to_dmy (* @@ to_gregorian_aux src *) d in
          match unbox_string @@ dst with
          | "Dgregorian" -> convert (fun x -> x)
          | "Djulian" -> convert Calendar.julian_of_gregorian
          | "Dfrench" -> convert Calendar.french_of_gregorian
          | "Dhebrew" -> convert Calendar.hebrew_of_gregorian
          | s -> failwith @@ "Unknown calendar: " ^ s
        )
      | "compare" -> date_compare
      | "death_symbol" -> Tstr death_symbol
      | "code_french_year" -> code_french_year
      | "eq" -> date_eq
      | "now" -> now
      | "string_of_age" -> string_of_age
      | "string_of_ondate" -> string_of_ondate
      | "sub" -> func_arg2_no_kw (fun d1 d2 -> mk_dmy @@ CheckItem.time_elapsed (to_dmy d2) (to_dmy d1))
      | _ -> raise Not_found
    )

and get_n_mk_person conf base (i : Adef.iper) =
  unsafe_mk_person conf base (Util.pget conf base i)

and mk_relation conf base r =
  let module E = Ezgw.Relation in
  let get wrap fn = try wrap (fn r) with Not_found -> Tnull in
  let has_relation_her = get box_bool E.has_relation_her in
  let has_relation_him = get box_bool E.has_relation_him in
  let related = get (get_n_mk_person conf base) @@ fun r -> Ezgw.Related.iper @@ E.related r in
  let related_type = get box_string (E.related_type conf) in
  let relation_type = get box_string (E.relation_type conf) in
  let relation_her = get (get_n_mk_person conf base) @@ fun r -> Ezgw.Related.iper @@ E.relation_her r in
  let relation_him = get (get_n_mk_person conf base) @@ fun r -> Ezgw.Related.iper @@ E.relation_him r in
  Tpat (function
      | "has_relation_her" -> has_relation_her
      | "has_relation_him" -> has_relation_him
      | "related" -> related
      | "related_type" -> related_type
      | "relation_type" -> relation_type
      | "relation_her" -> relation_her
      | "relation_him" -> relation_him
      | _ -> raise Not_found
    )

and mk_related conf base r =
  let module E = Ezgw.Related in
  let iper_ = E.iper r in
  let iper = Tint (Adef.int_of_iper iper_) in
  let person = Tlazy (lazy (get_n_mk_person conf base iper_)) in
  let type_ = Tstr (E.type_ conf r) in
  Tpat (function
      | "type" -> type_
      | "iper" -> iper
      | "person" -> person
      | _ -> raise Not_found
    )

and mk_witness_kind = function
  | Def.Witness -> Tstr "WITNESS"
  | Def.Witness_GodParent -> Tstr "WITNESS_GODPARENT"

and mk_event conf base d =
  let module E = Ezgw.Event in
  let date = match E.date d with Some d -> mk_date conf d | None -> Tnull in
  let name = Tstr (E.name conf base d) in
  let spouse =
    Tlazy (lazy (try unsafe_mk_person conf base @@ E.spouse base d
                 with Not_found -> Tnull))
  in
  let kind = Tstr (E.kind base d) in
  let witnesses =
    match E.witnesses d with
    | [||] -> Tarray [||]
    | w ->
      let lw = lazy (Array.map (fun (i, _) -> get_n_mk_person conf base i) w) in
      (* We may want to filter on [ip] or [k] before really accessing the person entity *)
      Tarray (Array.mapi (fun i (ip, k) ->
          let kind = mk_witness_kind k in
          let iper = Tint (Adef.int_of_iper ip) in
          Tpat (function
              | "kind" -> kind
              | "iper" -> iper
              | s -> unbox_pat (Lazy.force lw).(i) @@ s) )
          w )
  in
  let place = Tstr (E.place conf base d) in
  let src = Tstr (E.src base d) in
  let note = Tstr (E.note base d) in
  Tpat (function "date" -> date
               | "kind" -> kind
               | "name" -> name
               | "note" -> note
               | "place" -> place
               | "spouse" -> spouse
               | "src" -> src
               | "witnesses" -> witnesses
               | _ -> raise Not_found)

and mk_title conf base t =
  let ident = Tstr (Gwdb.sou base t.Def.t_ident) in
  let name = match t.t_name with
    | Tmain -> Tstr ""
    | Tname s -> Tstr (Gwdb.sou base s)
    | Tnone -> Tnull
  in
  let place = Tstr (Gwdb.sou base t.t_place) in
  let date_start = mk_opt (mk_date conf) (Adef.od_of_cdate t.t_date_start) in
  let date_end = mk_opt (mk_date conf) (Adef.od_of_cdate t.t_date_start) in
  let nth = Tint t.t_nth in
  Tpat (function
      | "ident" -> ident
      | "name" -> name
      | "place" -> place
      | "date_start" -> date_start
      | "date_end" -> date_end
      | "nth" -> nth
      | _ -> raise Not_found)

and unsafe_mk_person conf base (p : Gwdb.person) =
  (* match Perso_link.get_father_link conf.command (get_key_index a) with
   * | Some fath ->
   *   let ep = Perso_link.make_ep_link base fath in
   *   let conf = {conf with command = fath.MLink.Person.baseprefix} in
   *   let env = ("p_link", Vbool true) :: env in
   *   eval_person_field_var conf base env ep loc sl
   * | None ->
   *   warning_use_has_parents_before_parent loc "father" (str_val "") *)
  let get wrap fn = try wrap (fn p) with Not_found -> Tnull in
  let get_str = get box_string in
  let get_bool = get box_bool in
  let get_int = get box_int in
  let get_float = get box_float in
  let module E = Ezgw.Person in
  let parents =
    lazy (match E.parents p with
        | Some ifam -> Some (Gwdb.foi base ifam)
        | None -> None)
  in
  let mk_parent fn =
    Tlazy (lazy (match Lazy.force parents with
        | Some f -> get_n_mk_person conf base (fn f)
        | None -> Tnull (* fixme *) ) )
  in
  let iper' = Gwdb.get_key_index p in
  let access = get_str (E.access conf base) in
  let age = get mk_dmy (E.age conf) in
  let baptism_date = mk_opt (mk_date conf) (E.baptism_date p) in
  let baptism_place = get_str (E.baptism_place conf base) in
  let birth_date = mk_opt (mk_date conf) (E.birth_date p) in
  let birth_place = get_str (E.birth_place conf base) in
  let burial = get (mk_burial conf) E.burial in
  let burial_place = get_str (E.burial_place conf base) in
  let children =
    box_lazy @@
    lazy (box_list @@
          List.map (get_n_mk_person conf base) (E.children base p))
  in
  let consanguinity = get_float (E.consanguinity) in
  let cremation_place = get_str (E.cremation_place conf base) in
  let date = get_str (Date.short_dates_text conf base) in
  let dates = get_str (E.dates conf base) in
  let death = get (mk_death conf) E.death in
  let death_age = get_str (E.death_age conf) in
  let death_place = get_str (E.death_place conf base) in
  let died = get_str (E.died conf) in
  let digest = Tlazy (lazy (get_str (E.digest base) ) ) in
  let events = Tlazy (lazy (Tlist (E.events conf base p |> List.map (mk_event conf base)))) in
  let lazy_families = lazy (Array.map (fun ifam -> ifam, Gwdb.foi base ifam) @@ Gwdb.get_family p) in
  let families =
    Tlazy (lazy (Tarray (Array.map (fun (ifam, cpl) -> get_n_mk_family conf base ~origin:iper' ifam cpl) @@
                         Lazy.force lazy_families) ) )
  in
  let spouses =
    Tlazy (lazy (Tarray (Array.map (fun (_, c) ->
        let f = Gwdb.get_father c in
        get_n_mk_person conf base (if f = iper' then Gwdb.get_mother c else f) )
        (Lazy.force lazy_families) ) ) )
  in
  let father = mk_parent Gwdb.get_father in
  let first_name = get_str (E.first_name base) in
  let first_name_aliases =
    box_list @@
    List.map box_string (E.first_name_aliases base p)
  in
  let first_name_key = get_str (E.first_name_key base) in
  let first_name_key_val = get_str (E.first_name_key_val base) in
  let image_url = get_str (fun p -> E.image_url conf base p) in
  let iper = Tint (Adef.int_of_iper iper') in
  let is_birthday = get_bool (E.is_birthday conf) in
  let linked_page =
    box_lazy @@
    lazy (let fn = E.linked_page conf base p in Tpat (fun s -> Tstr (fn s) ) )
  in
  let max_ancestor_level = Tlazy (lazy (get_int (E.max_ancestor_level conf base) ) ) in
  let mother = mk_parent Gwdb.get_mother in
  let nb_families = get_int (E.nb_families conf) in
  let nobility_titles =
    box_lazy @@
    lazy (box_list @@ List.map (mk_title conf base) @@ E.nobility_titles conf base p)
  in
  let occ = get_int E.occ in
  let occupation = get_str (E.occupation conf base) in
  let on_baptism_date = get_str (E.on_baptism_date conf) in
  let on_birth_date = get_str (E.on_birth_date conf) in
  let on_burial_date = get_str (E.on_burial_date conf) in
  let on_cremation_date = get_str (E.on_cremation_date conf) in
  let on_death_date = get_str (E.on_death_date conf) in
  let public_name = get_str (E.public_name base) in
  let qualifier = get_str (E.qualifier base) in
  let qualifiers =
    Tlist (List.map box_string @@ E.qualifiers base p)
  in
  let related =
    box_lazy @@
    lazy (box_list @@
          List.map (fun (a, b) -> mk_relation conf base (b, Some a)) @@ (* FIXME? *)
          E.related conf base p)
  in
  let relations =
    match E.relations p with
    | [] -> Tlist []
    | r -> box_lazy @@ lazy (box_list @@ List.map (get_n_mk_person conf base) r)
  in
  let sex = get_int E.sex in
  let siblings_aux fn =
    Tlazy (lazy (Tlist (fn base p |> List.map (fun i -> Tlazy (lazy (get_n_mk_person conf base i))))))
  in
  let siblings = siblings_aux E.siblings in
  let half_siblings = siblings_aux E.half_siblings in
  let source_baptism = get_str @@ E.source_baptism base in
  let source_birth = get_str @@ E.source_birth base in
  let source_burial = get_str @@ E.source_burial base in
  let source_death = get_str @@ E.source_death base in
  let source_fsource =
    box_lazy @@ lazy (box_array @@ Array.map box_string @@ E.source_fsource conf base p)
  in
  let source_marriage =
    box_lazy @@ lazy (box_array @@ Array.map box_string @@ E.source_marriage conf base p)
  in
  let source_psources = get_str @@ E.source_psources base in
  let static_max_ancestor_level =
    box_lazy @@
    lazy (get_int ( (E.static_max_ancestor_level conf base) ) )
  in
  let str__ =
    box_lazy @@
    lazy (get_str (Util.person_text conf base)) (* FIXME *)
  in
  let surname_key_val = get_str (E.surname_key_val base) in
  let surname = get_str (E.surname base) in
  let surname_aliases = Tlist (List.map box_string (E.surname_aliases base p) ) in
  let surname_key = get_str (E.surname_key base) in
  Tlazy (lazy (Tpat
                 (function
                   | "access" -> access
                   | "age" -> age
                   | "baptism_date" -> baptism_date
                   | "baptism_place" -> baptism_place
                   | "birth_date" -> birth_date
                   | "birth_place" -> birth_place
                   | "burial" -> burial
                   | "burial_place" -> burial_place
                   | "children" -> children
                   | "cremation_place" -> cremation_place
                   | "consanguinity" -> consanguinity
                   | "date" -> date
                   | "dates" -> dates
                   | "death" -> death
                   | "death_age" -> death_age
                   | "death_place" -> death_place
                   | "died" -> died
                   | "digest" -> digest
                   | "events" -> events
                   | "families" -> families
                   | "father" -> father
                   | "first_name" -> first_name
                   | "first_name_aliases" -> first_name_aliases
                   | "first_name_key" -> first_name_key
                   | "first_name_key_val" -> first_name_key_val
                   | "half_siblings" -> half_siblings
                   | "image_url" -> image_url
                   | "iper" -> iper
                   | "is_birthday" -> is_birthday
                   | "linked_page" -> linked_page
                   | "max_ancestor_level" -> max_ancestor_level
                   | "mother" -> mother
                   | "nb_families" -> nb_families
                   | "occ" -> occ
                   | "occupation" -> occupation
                   | "on_baptism_date" -> on_baptism_date
                   | "on_birth_date" -> on_birth_date
                   | "on_burial_date" -> on_burial_date
                   | "on_cremation_date" -> on_cremation_date
                   | "on_death_date" -> on_death_date
                   | "public_name" -> public_name
                   | "qualifier" -> qualifier
                   | "qualifiers" -> qualifiers
                   | "relations" -> relations
                   | "related" -> related
                   | "sex" -> sex
                   | "siblings" -> siblings
                   | "source_baptism" -> source_baptism
                   | "source_birth" -> source_birth
                   | "source_burial" -> source_burial
                   | "source_death" -> source_death
                   | "source_fsource" -> source_fsource
                   | "source_marriage" -> source_marriage
                   | "source_psources" -> source_psources
                   | "spouses" -> spouses
                   | "static_max_ancestor_level" -> static_max_ancestor_level
                   | "surname" -> surname
                   | "surname_aliases" -> surname_aliases
                   | "surname_key" -> surname_key
                   | "surname_key_val" -> surname_key_val
                   | "titles" -> nobility_titles
                   | "__str__" -> str__
                   | _ -> raise Not_found
                 )
              )
        )

(* FIXME *)
and mk_source _s =
  let source_type = Tstr "" in
  let str__ = Tfun (fun ?kwargs:_ _ -> Tstr "") in
  Tpat (function
      | "source_type" -> source_type
      | "__str__" -> str__
      | _ -> raise Not_found
    )

and mk_time (hh, mm, ss) =
  Tpat (function
      | "__str__" -> Tstr (Printf.sprintf "%02d:%02d:%02d" hh mm ss)
      | "hours" -> Tstr (Printf.sprintf "%02d" hh)
      | "minutes" -> Tstr (Printf.sprintf "%02d" mm)
      | "seconds" -> Tstr (Printf.sprintf "%02d" ss)
      | _ -> raise Not_found)

and mk_dmy { Def.day ; month ; year ; delta ; prec } =
  let day = Tint day in
  let month = Tint month in
  let year = Tint year in
  let delta = Tint delta in
  let prec = Tstr (to_prec prec) in
  Tpat (function
      | "day" -> day
      | "month" -> month
      | "year" -> year
      | "delta" -> delta
      | "prec" -> prec
      | _ -> raise Not_found
    )

(* TODO: remove and replace with generic mk_event *)
and mk_fevent conf base e =
  Tpat (function "efam_name" -> Tstr (Util.string_of_fevent_name conf base e.Def.efam_name)
               | _ -> raise Not_found)

(* TODO: remove and replace with generic mk_event *)
and mk_pevent conf base e =
  let epers_name = Tstr (Util.string_of_pevent_name conf base e.Def.epers_name) in
  Tpat (function "epers_name" -> epers_name
               | "date" -> Tnull
               | "family" -> Tnull
               | "note" -> Tnull
               | "place" -> Tnull
               | "witnesses" -> Tnull
               | _ -> raise Not_found)

and mk_gen_title conf base t =
  Tpat (function "t_ident" -> Tstr (Gwdb.sou base t.Def.t_ident)
               | "t_place" -> Tstr (Gwdb.sou base t.t_place)
               | "t_date_start" ->
                 begin match Adef.od_of_cdate t.t_date_start with
                   | Some d -> mk_date conf d
                   | None -> Tnull
                 end
               | "t_date_end" ->
                 begin match Adef.od_of_cdate t.t_date_end with
                   | Some d -> mk_date conf d
                   | None -> Tnull
                 end
               | _ -> raise Not_found)

and mk_death_reason = function
  | Def.Killed -> Tstr "Killed"
  | Murdered -> Tstr "Murdered"
  | Executed -> Tstr "Executed"
  | Disappeared -> Tstr "Disappeared"
  | Unspecified -> Tstr "Unspecified"

and mk_death conf =
  let wrap s = Tpat (function "death_reason" -> Tstr s | _ -> raise Not_found) in
  function
  | Def.NotDead -> Tnull
  | Death (r, cd) ->
    let death_reason = mk_death_reason r in
    let date = mk_date conf (Adef.date_of_cdate cd) in
    Tpat (function "death_reason" -> death_reason
                 | "date" -> date
                 | _ -> raise Not_found)
  | DeadYoung -> wrap "DeadYoung"
  | DeadDontKnowWhen -> wrap "DeadDontKnowWhen"
  | DontKnowIfDead -> wrap "DontKnowIfDead"
  | OfCourseDead -> wrap "OfCourseDead"

and mk_burial conf = function
  | Def.UnknownBurial -> Tnull
  | Buried d ->
    let type_ = Tstr "Buried" in
    let date = match Adef.od_of_cdate d with
      | Some d -> mk_date conf d
      | None -> Tnull
    in
    Tpat (function "type" -> type_
                 | "date" -> date
                 | _ -> raise Not_found)
  | Cremated d ->
    let type_ = Tstr "Cremated" in
    let date = match Adef.od_of_cdate d with
      | Some d -> mk_date conf d
      | None -> Tnull
    in
    Tpat (function "type" -> type_
                 | "date" -> date
                 | _ -> raise Not_found)

(* take optionnal p parameter for spouse things? *)
and mk_warning conf base =
  let get_fam ifam =
    let cpl = Gwdb.foi base ifam in
    let ifath = Gwdb.get_father cpl in
    let imoth = Gwdb.get_mother cpl in
    (* spouse if not used so it should be okay *)
    mk_family conf base (ifam, cpl, (ifath, imoth, imoth), true)
  in
  let array_of_list_map : 'a 'b . ('a -> 'b) -> 'a list -> 'b array =
    fun fn l ->
    if l = [] then [||] else begin
      let a = Array.make (List.length l) (fn @@ List.hd l) in (* FIXME *)
      List.iteri (fun i x -> a.(i) <- fn x) l ;
      a
    end
  in
  function
  | Def.BigAgeBetweenSpouses (f, m, a) ->
    Tset [ Tstr "BigAgeBetweenSpouses"
         ; Tset [ unsafe_mk_person conf base f
                ; unsafe_mk_person conf base m
                ; mk_date conf (Dgreg (a, Dgregorian) ) ] ] (* gregorian?? *)
  | BirthAfterDeath p ->
    Tset [ Tstr "BirthAfterDeath" ; Tset [ unsafe_mk_person conf base p] ]
  | IncoherentSex (p, i1, i2) ->
    Tset [ Tstr "BirthAfterDeath"
         ; Tset [ unsafe_mk_person conf base p
                ; Tint i1
                ; Tint i2 ] ]
  | ChangedOrderOfChildren (ifam, _descend, before, after) ->
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfChildren"
         ; Tset [ get_fam ifam
                ; Tarray (Array.map (get_n_mk_person conf base) before)
                ; Tarray (Array.map (get_n_mk_person conf base) after)
                ; Tarray (Array.map box_bool bef_d)
                ; Tarray (Array.map box_bool aft_d)
                ] ]
  | ChangedOrderOfMarriages (p, before, after) ->
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfMarriages"
         ; Tset [ unsafe_mk_person conf base p
                ; Tarray (Array.map get_fam before)
                ; Tarray (Array.map get_fam after)
                ; Tarray (Array.map box_bool bef_d)
                ; Tarray (Array.map box_bool aft_d)
                ] ]
  | ChangedOrderOfFamilyEvents (_ifam, before, after) ->
    let before = array_of_list_map (mk_fevent conf base) before in
    let after = array_of_list_map (mk_fevent conf base) after in
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfFamilyEvents"
         ; Tset [ Tarray before
                ; Tarray after
                ; Tarray (Array.map box_bool bef_d)
                ; Tarray (Array.map box_bool aft_d)
                ] ]
  | ChangedOrderOfPersonEvents (_p, before, after) ->
    let before = array_of_list_map (mk_pevent conf base) before in
    let after = array_of_list_map (mk_pevent conf base) after in
    let (bef_d, aft_d) = Difference.f before after in
    Tset [ Tstr "ChangedOrderOfPersonEvents"
         ; Tset [ Tarray before
                ; Tarray after
                ; Tarray (Array.map box_bool bef_d)
                ; Tarray (Array.map box_bool aft_d)
                ] ]
  | ChildrenNotInOrder (ifam, _descend, elder, x) ->
    Tset [ Tstr "ChildrenNotInOrder"
         ; Tset [ get_fam ifam
                ; unsafe_mk_person conf base elder
                ; unsafe_mk_person conf base x
                ]
         ]
  | CloseChildren (ifam, _descend, elder, x) ->
    Tset [ Tstr "CloseChildren"
         ; Tset [ get_fam ifam
                ; unsafe_mk_person conf base elder
                ; unsafe_mk_person conf base x
                ]
         ]
  | DeadOld (p, a) ->
    Tset [ Tstr "DeadOld"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_date conf (Dgreg (a, Dgregorian) ) ] ] (* gregorian?? *)
  | DeadTooEarlyToBeFather (father, child) ->
    Tset [ Tstr "DeadTooEarlyToBeFather"
         ; Tset [ unsafe_mk_person conf base father
                ; unsafe_mk_person conf base child ] ] (* gregorian?? *)
  | FEventOrder (p, e1, e2) ->
    Tset [ Tstr "FEventOrder"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_fevent conf base e1
                ; mk_fevent conf base e2 ] ]
  | FWitnessEventAfterDeath (p, e) ->
    Tset [ Tstr "FWitnessEventAfterDeath"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_fevent conf base e ] ]
  | FWitnessEventBeforeBirth (p, e) ->
    Tset [ Tstr "FWitnessEventBeforeBirth"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_fevent conf base e ] ]
  | IncoherentAncestorDate (p1, p2) ->
    Tset [ Tstr "IncoherentAncestorDate"
         ; Tset [ unsafe_mk_person conf base p1
                ; unsafe_mk_person conf base p2 ] ]
  | MarriageDateAfterDeath p ->
    Tset [ Tstr "MarriageDateAfterDeath"
         ; Tset [ unsafe_mk_person conf base p ] ]
  | MarriageDateBeforeBirth p ->
    Tset [ Tstr "MarriageDateBeforeBirth"
         ; Tset [ unsafe_mk_person conf base p ] ]
  | MotherDeadAfterChildBirth (p1, p2) ->
    Tset [ Tstr "MotherDeadAfterChildBirth"
         ; Tset [ unsafe_mk_person conf base p1
                ; unsafe_mk_person conf base p2 ] ]
  | ParentBornAfterChild (p1, p2) ->
    Tset [ Tstr "ParentBornAfterChild"
         ; Tset [ unsafe_mk_person conf base p1
                ; unsafe_mk_person conf base p2 ] ]
  | ParentTooOld (p, a) ->
    Tset [ Tstr "ParentTooOld"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_date conf (Dgreg (a, Dgregorian) ) ] ] (* gregorian?? *)
  | ParentTooYoung (p, a) ->
    Tset [ Tstr "ParentTooYoung"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_date conf (Dgreg (a, Dgregorian) ) ] ] (* gregorian?? *)
  | PEventOrder (p, e1, e2) ->
    Tset [ Tstr "PEventOrder"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_pevent conf base e1
                ; mk_pevent conf base e2 ] ]
  | PWitnessEventAfterDeath (p, e) ->
    Tset [ Tstr "PWitnessEventAfterDeath"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_pevent conf base e ] ]
  | PWitnessEventBeforeBirth (p, e) ->
    Tset [ Tstr "PWitnessEventBeforeBirth"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_pevent conf base e ] ]
  | TitleDatesError (p, t) ->
    Tset [ Tstr "PWitnessEventBeforeBirth"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_gen_title conf base t ] ]
  | UndefinedSex p ->
    Tset [ Tstr "UndefinedSex"
         ; Tset [ unsafe_mk_person conf base p ] ]
  | WitnessDateAfterDeath p ->
    Tset [ Tstr "WitnessDateAfterDeath"
         ; Tset [ unsafe_mk_person conf base p ] ]
  | WitnessDateBeforeBirth p ->
    Tset [ Tstr "WitnessDateBeforeBirth"
         ; Tset [ unsafe_mk_person conf base p ] ]
  | YoungForMarriage (p, a) ->
    Tset [ Tstr "YoungForMarriage"
         ; Tset [ unsafe_mk_person conf base p
                ; mk_date conf (Dgreg (a, Dgregorian) ) ] ] (* gregorian?? *)

  | PossibleDuplicateFam _ -> assert false (* FIXME *)

let mk_conf conf base =
  let _commd_no_params = Tnull in (* FIXME *)
  let link_to_referer = Tstr (Hutil.link_to_referer conf) in (* TO BE REMOVED? *)
  let from = Tstr conf.Config.from in
  let api_host = Tstr conf.api_host in
  let api_port = Tint conf.api_port in
  let manitou = Tbool conf.manitou in
  let supervisor = Tbool conf.supervisor in
  let wizard = Tvolatile (fun () -> Tbool conf.wizard) in
  let is_printed_by_template = Tbool conf.is_printed_by_template in
  let friend = Tbool conf.friend in
  let just_friend_wizard = Tbool conf.just_friend_wizard in
  let user = Tstr conf.user in
  let username = Tstr conf.username in
  let auth_scheme = Tnull (* auth_scheme : auth_scheme_kind; *) in
  let pure_xhtml = Tbool conf.pure_xhtml in
  let command = Tstr conf.command in
  let indep_command = Tstr conf.indep_command in
  let highlight = Tstr conf.highlight in
  let lang = Tstr conf.lang in
  let default_lang = Tstr conf.default_lang in
  let default_sosa_ref =
    let (iper, _p) = conf.default_sosa_ref in
    let person = match Adef.int_of_iper iper with
      | -1 -> Tnull
      | _ -> Tlazy (lazy (get_n_mk_person conf base iper) )
    in
    Tobj [ ( "iper", Tint (Adef.int_of_iper iper))
         ; ( "person", person ) ] in
  let multi_parents = Tbool conf.multi_parents in
  let can_send_image = Tbool conf.can_send_image in
  let authorized_wizards_notes = Tbool conf.authorized_wizards_notes in
  let public_if_titles = Tbool conf.public_if_titles in
  let public_if_no_date = Tbool conf.public_if_no_date in
  let setup_link = Tvolatile (fun () -> Tbool conf.setup_link) in
  let accessByKey = Tbool conf.access_by_key in
  let private_years = Tint conf.private_years in
  let hide_names = Tbool conf.hide_names in
  let use_restrict = Tbool conf.use_restrict in
  let no_image = Tbool conf.no_image in
  let no_note = Tbool conf.no_note in
  let bname = Tstr conf.bname in
  let cgi_passwd = Tstr conf.cgi_passwd in
  let env = Tobj (List.map (fun (k, v) -> (k, Tstr v)) conf.env) in
  let senv = Tlazy (lazy (Tobj (List.map (fun (k, v) -> (k, Tstr v)) conf.senv))) in
  let henv = Tlazy (lazy (Tobj (List.map (fun (k, v) -> (k, Tstr v)) conf.henv))) in
  let benv = Tlazy (lazy (Tobj (List.map (fun (k, v) -> (k, Tstr v)) conf.base_env))) in
  let allowed_titles =
    Tlazy (lazy (Tlist (List.map (fun x -> Tstr x) (Lazy.force conf.allowed_titles) ) ) )
  in
  let denied_titles =
    Tlazy (lazy (Tlist (List.map (fun x -> Tstr x) (Lazy.force conf.denied_titles) ) ) )
  in
  let xhs = Tstr conf.xhs in
  let request = Tlist (List.map (fun x -> Tstr x) conf.request) in
  let lexicon = Tpat (fun s -> Tstr (Hashtbl.find conf.lexicon s) ) in
  let charset = Tvolatile (fun () -> Tstr conf.charset) in
  let is_rtl = Tbool conf.is_rtl in
  let left = Tstr conf.left in
  let right = Tstr conf.right in
  let auth_file = Tstr conf.auth_file in
  let border = Tint conf.border in
  let n_connect = Tnull (* FIXME *) in
  let today = mk_dmy conf.today in
  let todayWd = Tint conf.today_wd in
  let time = mk_time conf.time in
  let ctime = Tfloat conf.ctime in
  let image_prefix = Tstr "https://gw.geneanet.org/images/"(* conf.image_prefix *) in
  let bArgForBasename = Tbool conf.b_arg_for_basename in
  Tpat (function
      | "access_by_key" -> accessByKey
      | "allowed_titles" -> allowed_titles
      | "api_host" -> api_host
      | "api_port" -> api_port
      | "auth_file" -> auth_file
      | "auth_scheme" -> auth_scheme
      | "authorized_wizards_notes" -> authorized_wizards_notes
      | "b_arg_for_basename" -> bArgForBasename
      | "benv" -> benv
      | "bname" -> bname
      | "border" -> border
      | "can_send_image" -> can_send_image
      | "cgi_passwd" -> cgi_passwd
      | "charset" -> charset
      | "command" -> command
      | "ctime" -> ctime
      | "default_lang" -> default_lang
      | "default_sosa_ref" -> default_sosa_ref
      | "denied_titles" -> denied_titles
      | "env" -> env
      | "friend" -> friend
      | "from" -> from
      | "henv" -> henv
      | "hide_names" -> hide_names
      | "highlight" -> highlight
      | "image_prefix" -> image_prefix
      | "indep_command" -> indep_command
      | "is_printed_by_template" -> is_printed_by_template
      | "is_rtl" -> is_rtl
      | "just_friend_wizard" -> just_friend_wizard
      | "lang" -> lang
      | "left" -> left
      | "lexicon" -> lexicon
      | "link_to_referer" -> link_to_referer
      | "manitou" -> manitou
      | "multi_parents" -> multi_parents
      | "n_connect" -> n_connect
      | "no_image" -> no_image
      | "no_note" -> no_note
      | "private_years" -> private_years
      | "public_if_no_date" -> public_if_no_date
      | "public_if_titles" -> public_if_titles
      | "pure_xhtml" -> pure_xhtml
      | "request" -> request
      | "right" -> right
      | "senv" -> senv
      | "setup_link" -> setup_link
      | "supervisor" -> supervisor
      | "time" -> time
      | "today" -> today
      | "today_wd" -> todayWd
      | "use_restrict" -> use_restrict
      | "user" -> user
      | "username" -> username
      | "wizard" -> wizard
      | "xhs" -> xhs
      | _ -> raise Not_found
    )

let mk_env conf =
  (* FIXME browsing_with_sosa_ref *)
  let compilation_time = Tstr (Date.string_of_date conf Compilation.compilation_time) in
  let commit = Tstr Compilation.commit in
  let commit_date = Tstr (Date.string_of_date conf Compilation.commit_date) in
  let doctype = Tstr (Util.doctype conf) in
  let get = Tpat (fun x -> Tstr (List.assoc x conf.env)) in
  let highlight = Tstr (conf.Config.highlight) in
  let image_prefix = Tstr (Util.image_prefix conf) in
  let prefix = Tstr (Util.commd conf) in
  let prefix_base = Tstr (Util.prefix_base conf) in
  let prefix_no_iz =
    let henv =
      List.fold_left (fun accu k -> List.remove_assoc k accu) conf.henv
        ["iz"; "nz"; "pz"; "ocz"]
    in
    Tstr (Util.commd {conf with henv = henv})
  in
  let referer = Tstr (Util.get_referer conf) in
  let version = Tstr Version.txt in
  let wo_henv_senv =
    let l =
      List.fold_left
        (fun accu (k, _) -> List.remove_assoc k accu)
        (List.fold_left
           (fun accu (k, _) -> List.remove_assoc k accu)
           conf.env
           conf.henv)
        conf.senv
    in
    fun s -> Tstr (List.fold_left (fun c (k, v) -> c ^ k ^ "=" ^ v ^ "&") s l)
  in
  let suffix = wo_henv_senv "" in
  let url = wo_henv_senv (Util.commd conf) in
  Tpat (function
      | "compilation_time" -> compilation_time
      | "commit" -> commit
      | "commit_date" -> commit_date
      | "doctype" -> doctype
      | "get" -> get
      | "highlight" -> highlight
      | "image_prefix" -> image_prefix
      | "prefix" -> prefix
      | "prefix_base" -> prefix_base
      | "prefix_no_iz" -> prefix_no_iz
      | "referer" -> referer
      | "suffix" -> suffix
      | "url" -> url
      | "version" -> version
      | _ -> raise Not_found
    )

(* TODO: remove base *)
let translate conf (* base *) =
  let decline = func_arg2_no_kw @@ fun s1 s2 ->
    try Tstr (Util.transl_decline conf (unbox_string s1) (unbox_string s2))
    with _ -> failwith_type_error_2 "translate" s1 s2
  in
  let nth = func_arg2_no_kw @@ fun a1 a2 ->  match a1, a2 with
    | Tstr s, Tint i -> Tstr (Util.transl_nth conf s i)
    | Tstr s, Tstr i -> Tstr (Util.transl_nth conf s @@ int_of_string i)
    | _ -> failwith_type_error_2 "nth" a1 a2
  in
  let transl_a_of_b = func_arg2_no_kw @@ fun x y ->
    try Tstr (Util.transl_a_of_b conf (unbox_string x) (unbox_string y))
    with _ -> failwith_type_error_2 "a_of_b" x y
  in
  let transl_a_of_gr_eq_gen_lev = func_arg2_no_kw @@ fun x y ->
    try Tstr (Util.transl_a_of_gr_eq_gen_lev conf (unbox_string x) (unbox_string y))
    with _ -> failwith_type_error_2 "a_of_gr_eq_gen_lev" x y
  in
  let transl = func_arg1_no_kw @@ fun x ->
    try Tstr (Util.transl conf (unbox_string x))
    with _ -> failwith_type_error_1 "transl" x
  in
  let ftransl = func_arg2_no_kw @@ fun x y ->
    match x, y with
    | Tstr s, Tint i -> Tstr (Printf.sprintf (Scanf.format_from_string (Util.transl conf s) "%d") i)
    | Tstr s, Tstr s' -> Tstr (Printf.sprintf (Scanf.format_from_string (Util.transl conf s) "%s") s')
    | _ -> failwith_type_error_2 "ftransl" x y
  in
  Tpat (function "decline" -> decline
               | "nth" -> nth
               | "transl" -> transl
               | "transl_a_of_b" -> transl_a_of_b
               | "transl_a_of_gr_eq_gen_lev" -> transl_a_of_gr_eq_gen_lev
               | "ftransl" -> ftransl
               | x -> failwith x)

let decode_varenv =
  func_arg1_no_kw @@ fun str ->
  try Tstr (Wserver.decode @@ unbox_string str)
  with _ -> failwith_type_error_1 "decode_varenv" str

let code_varenv =
  func_arg1_no_kw @@ fun str ->
  try Tstr (Wserver.encode @@ unbox_string str)
  with _ -> failwith_type_error_1 "decode_varenv" str

let mk_evar conf =
  Tpat (fun v -> match Util.p_getenv (conf.Config.env @ conf.henv) v with
      | Some vv -> Tstr ((* Util.escape_html *) vv)
      | None -> Tnull)

let mk_base base =
  Tpat (function
      | "nb_of_persons" -> Tint (Gwdb.nb_of_persons base)
      | "nb_of_families" -> Tint (Gwdb.nb_of_families base)
      | _ -> raise Not_found
    )

(**
   [{{ 'foo' | trans }}], [{{ "foo" | trans }}], [{{ trans ("foo") }}]
   all get converted to [{{ 'foo' | trans }}].

   If the argument (['foo']) contains a ['], double quotes are used.
  *)
let trans =
  func_arg1_no_kw @@ function
  | Tstr s -> Tstr (Printf.sprintf
                      (if String.contains s '\'' then "{{ \"%s\" | trans }}" else "{{ '%s' | trans }}") s)
  | x -> failwith_type_error_1 "trans" x

let default_env conf base (* p *) =
  let conf_env = mk_conf conf base in
  (* FIXME: remove this *)
  (* let initCache = Tfun (fun ?kwargs:_ args -> match args with
   *     | [ p ; Tint nb_asc ; Tint from_gen_desc ; Tint nb_desc ] ->
   *       Geneweb.Perso_link.init_cache
   *         conf base (Gwdb.get_key_index p) nb_asc from_gen_desc nb_desc ;
   *       Tnull
   *     | _ -> assert false)
   * in *)
  let evar = mk_evar conf in
  ("conf", conf_env)
  :: ("trans", trans)
  :: ("DATE", module_date conf)
  :: ("env", mk_env conf)
  :: ("evar", evar)
  (* :: ("initCache", initCache) *)
  :: ("decode_varenv", decode_varenv)
  :: ("code_varenv", code_varenv)
  :: ("translate", translate conf)
  :: ("base", mk_base base)
  :: ("log", Tfun (fun ?kwargs:_ x -> Printf.eprintf "log: %s\n" @@ Jg_runtime.string_of_tvalue x ; Tnull))
  :: []

let sandbox (conf : Config.config) base =
  let die =
    let rec printer = function
      | Tint x -> Printf.sprintf "Tint %d" x
      | Tfloat x -> Printf.sprintf "Tfloat %f" x
      | Tstr x -> Printf.sprintf "Tstr %s" (String.escaped x)
      | Tbool x -> Printf.sprintf "Tbool %b" x
      | Tobj _ -> Printf.sprintf "<Tobj>"
      | Thash _ -> Printf.sprintf "<Thash>"
      | Tlist x -> Printf.sprintf "Tlist [ %s ]" (String.concat ";" @@ List.map printer x)
      | Tpat _ -> Printf.sprintf "<Tpat>"
      | Tset _ -> Printf.sprintf "<Tset>"
      | Tfun _ -> Printf.sprintf "<Tfun>"
      | Tnull -> Printf.sprintf "Tnull"
      | Tarray x -> Printf.sprintf "Tarray [| %s |]" (String.concat ";" @@ List.map printer @@ Array.to_list x)
      | Tlazy _ -> Printf.sprintf "Tlazy"
      | Tvolatile _ -> Printf.sprintf "Tvolatile"
    in
    func_arg1_no_kw @@ fun x -> Tstr (printer x)
  in
  let get_person = func_arg1_no_kw @@ function
    | Tint i -> get_n_mk_person conf base (Adef.iper_of_int i)
    | x -> failwith_type_error_1 "GET_PERSON" x
  in
  let get_family = func_arg1_no_kw @@ function
    | Tint i ->
      let ifam = Adef.ifam_of_int i in
      let cpl = Gwdb.foi base ifam in
      get_n_mk_family conf base ifam cpl
    | x -> failwith_type_error_1 "GET_FAMILY" x
  in
  let () = Random.self_init () in
  ("DIE", die)
  :: ("GET_PERSON", get_person)
  :: ("GET_FAMILY", get_family)
  :: ("RANDOM_IPER", Tvolatile (fun () -> Tint (Random.int (Gwdb.nb_of_persons base))))
  :: ("RANDOM_IFAM", Tvolatile (fun () -> Tint (Random.int (Gwdb.nb_of_families base))))
  :: ("DATE", module_date conf)
  :: ("trans", trans)
  :: ("printf", Jg_types.func_arg1_no_kw Jg_runtime.jg_printf)
  :: default_env conf base
